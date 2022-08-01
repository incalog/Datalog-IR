package inca.frontend.functional.lowering

import inca.backend.hints.DataHints
import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.hints.MagicSetHints
import inca.backend.hints.MagicSetHints.FixedAdornment
import inca.backend.hints.MagicSetHints.IgnoreCall
import inca.backend.hints.MagicSetHints.NoInputRelation
import inca.backend.ir.DatalogScala
import inca.backend.ir.Substitute
import inca.frontend.functional.core._
import inca.runtime.data.MockURI
import inca.util.Gensym
import inca.util.Scala
import inca.util.Scala.symbolOf
import inca.util.Scala.typeOf
import inca.util.TupleOps
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.meta.quasiquotes._

object GenerateDatalog {
  def transformModule(module: Module): DatalogScala.Module =
    new GenerateDatalog(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[DatalogScala.Module] =
    modules.map(transformModule)
}

class GenerateDatalog(module: Module) {
  private val gensym: Gensym = new Gensym(Iterable.empty)
  private val genScala = new GenerateScala

  private val generatedPatterns = ListBuffer[DatalogScala.Pattern]()

  def transModule(): DatalogScala.Module = {
    val Module(name, imports, contents) = module
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedDefNames.map(_.name))

    contents.foreach {
      case fun: FunctionDef => generatedPatterns += transFun(fun)
      case data: DataDef =>
        generatedPatterns ++= transData(data)
        genScala.genDataDef(data)
    }

    DatalogScala.Module(
      name.name,
      imports.map(_.name.name),
      generatedPatterns.toList,
      genScala.generated.map(Scala.apply)
    ).addHint(SourceConstruct.from(module))
  }

  // needs to be reset before flattening params
  private var tupleParams: Map[DatalogScala.Name, Seq[DatalogScala.Name]] = Map()

  private def transFun(fun: FunctionDef): DatalogScala.Pattern = gensym.scoped {
    gensym.register(fun.vars.keys.map(_.name))

    val vis = transVis(fun.vis)
    // reset before flattening params
    tupleParams = Map()
    val params = fun.params.flatMap { p =>
      val res = flattenParam(p.name.name, p.typ, genFresh = false)
      if (res.size > 1) {
        tupleParams = tupleParams + (p.name.name -> res.map(_.name))
      }
      res
    }
    val outParams = flattenParam("out", fun.outType, genFresh = true)

    val bodies =
      for ((terms, cons) <- transExp(fun.body))
        yield DatalogScala.Body(
          cons ++ outParams.zip(terms).map(pt => DatalogScala.Eq(DatalogScala.Var(pt._1.name), pt._2))
        )

    val pat = DatalogScala.Pattern(vis, fun.name.name, params ++ outParams, bodies)
      .addHint(SourceConstruct.from(fun))
    if (fun.hasAnnotation(MainFunctionAnno.key))
      pat.addHint(MagicSetHints.Main(params.map(_ => true) ++ outParams.map(_ => false)))
    pat
  }

  private def flattenParam(name: String, typ: Type, genFresh: Boolean): Seq[DatalogScala.Param] =
    typ match {
      case TTuple(tys) =>
        tys.zipWithIndex.flatMap { case (ty, ix) =>
          flattenParam(name + "_" + ix, ty, genFresh = true)
        }
      case TOption(ty) => flattenParam(name, ty, genFresh)
      case TSet(ty) => flattenParam(name, ty, genFresh)
      case TNothing => Seq()
      case _ =>
        val v = if (genFresh) gensym.fresh(name) else name
        Seq(DatalogScala.Param(v, transType(typ)))
    }

  type ExpRes = Seq[(Seq[DatalogScala.Term], Seq[DatalogScala.Atom])]

  def generatePattern(exp: Expression, basename: String): DatalogScala.Pattern = {
    val name = gensym.freshGlobal(basename)
    val vars = exp.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }
    val params = vars.map { case (v, ty) => DatalogScala.Param(v.name, ty) }
    val expTys = exp.typ.getOrElse(
      throw new IllegalArgumentException(s"Cannot compile untyped expression $exp")
    ).flatten
    val outParams = expTys.map(ty => DatalogScala.Param(gensym.fresh("out"), transType(ty)))

    val bodies =
      for ((terms, cons) <- transExp(exp))
        yield DatalogScala.Body(
          cons ++ outParams.zip(terms).map(pt => DatalogScala.Eq(DatalogScala.Var(pt._1.name), pt._2))
        )

    DatalogScala.Pattern(None, name, params ++ outParams, bodies).addHint(SourceConstruct.from(exp))
  }

  private def flatVars(x: Name, ty: Type): Seq[(DatalogScala.Var, DatalogScala.Type)] = ty match {
    case TTuple(ts) =>
      ts.zipWithIndex.map { case (ty, ix) => DatalogScala.Var(x.name + "$_" + ix) -> transType(ty) }
      tupleParams.get(x.name) match {
        case Some(vars) =>
          ts.zip(vars).map { case (ty, v) => DatalogScala.Var(v) -> transType(ty) }
        case None =>
          ts.zipWithIndex.map { case (ty, ix) => DatalogScala.Var(x.name + "$_" + ix) -> transType(ty) }
      }
    case ty =>
      Seq(DatalogScala.Var(x.name) -> transType(ty))
  }

  private def transExp(exp: Expression): ExpRes = exp match {
    case Var(name) =>
      Seq(
        (
          flatVars(
            name,
            exp.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $exp"))
          ).map(_._1),
          Seq()
        )
      )

    case Let(names, _, bound, body) =>
      val tys = bound.typ.get match {
        case TTuple(tys) => tys
        case ty => Seq(ty)
      }
      val vars = names.zip(tys).flatMap { case (name, ty) =>
        flatVars(name, ty).map(_._1)
      }
      for {
        (boundTerms, boundCons) <- transExp(bound)
        (bodyTerm, bodyCons) <- transExp(body)
      } yield {
        val eqs = vars.zip(boundTerms).map(vt =>
          DatalogScala.Eq(vt._1, vt._2).addHint(SourceConstruct.from(exp, exp -> vt._1.name)))
        (bodyTerm, boundCons ++ eqs ++ bodyCons)
      }
    case TypeCast(e, ty) =>
      for ((Seq(eTerm), eCons) <- transExp(e)) yield {
        val instanceCall = DatalogScala.Call(ty.toString, Seq(eTerm))
        (Seq(eTerm), eCons :+ instanceCall)
      }

    case If(cnd, thn, els) =>
      val condTrans = transExp(cnd)
      val thnTrans = transExp(thn)
      val elsTrans = transExp(els)
      val thnRes: ExpRes =
        for {
          (Seq(cndTerm), cndCons) <- condTrans
          (thnTerm, thnCons) <- thnTrans
        } yield (
          thnTerm,
          cndCons ++ Seq(
            DatalogScala.Eq(cndTerm, DatalogScala.host.True).addHint(SourceConstruct.from(exp, exp -> true))
          ) ++ thnCons
        )
      val elsRes: ExpRes =
        for {
          (Seq(cndTerm), cndCons) <- condTrans
          (elsTerm, elsCons) <- elsTrans
        } yield (
          elsTerm,
          cndCons ++ Seq(
            DatalogScala.Eq(cndTerm, DatalogScala.host.False).addHint(SourceConstruct.from(exp, exp -> false))
          ) ++ elsCons
        )
      thnRes ++ elsRes

    case call @ Call(Var(name), args, transitive) =>
      if (name.name == "parent") {
        return for ((Seq(argTerm), argCons) <- transExp(args.head)) yield {
          // val argTy = transType(args.head.typ.get)
          val parentTerm = DatalogScala.Var(gensym.fresh("parent"))
          // val parentLinkCons = Datalog.Path(argTerm, argTy, Datalog.ParentLink, parentTerm, Datalog.TAny)
          // figure out what the dataDef is of the argument type
          val dataDef = args.head.typ.get match {
            case dty: TData => dty.target.get.asInstanceOf[DataDef]
            case _ => throw new IllegalStateException("Cannot happen")
          }
          val parentCall = DatalogScala.Call(dataDef.parentName, Seq(argTerm, parentTerm))
          (Seq(parentTerm), argCons :+ parentCall)
        }
      }

      val outvars = call.fun.typ match {
        case Some(TFun(_, outType)) => outType.flatten.map(_ => DatalogScala.Var(gensym.fresh("call")))
        case Some(outType) => Seq(DatalogScala.Var(gensym.fresh("call")))
        case None => throw new IllegalArgumentException(s"Untyped call $call")
      }
      val argRes = args.map(e => transExp(e))

      // create single call constraint when no arguments passed
      if (argRes.isEmpty)
        return Seq(
          (
            outvars,
            Seq(
              DatalogScala.Call(name.name, outvars, transitive, neg = false).addHint(
                SourceConstruct.from(call)
              )
            )
          )
        )

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (
          outvars,
          argCons.flatten ++ Seq(
            DatalogScala.Call(name.name, argTerms.flatten ++ outvars, transitive, neg = false).addHint(
              SourceConstruct.from(call)
            )
          )
        )
      }

    case Tuple(exps) =>
      if (exps.isEmpty)
        return Seq((Seq(), Seq()))

      val expRes = exps.map(e => transExp(e))
      for (tups <- TupleOps.cartesianProduct(expRes)) yield {
        val (terms, cons) = tups.unzip
        (terms.flatten, cons.flatten)
      }

    case Match(matchee, cases) =>
      val matcheeRes = transExp(matchee)
      for {
        (pat, body) <- cases
        (bodyTerms, bodyCons) <- transExp(body)
        (Seq(matcheeTerm), matcheeCons) <- matcheeRes
      } yield {
        val patCons = pat match {
          case cpat: ConstructorPattern =>
            val selector = cpat.target match {
              case Some(constr: DataConstructor) => constr.selectorName
              case Some(target) =>
                throw new IllegalStateException(s"Unknown constructor target $target")
              case None =>
                throw new IllegalArgumentException(
                  s"Cannot compile unresolved constructor pattern $cpat"
                )
            }
            DatalogScala.Call(selector, matcheeTerm +: cpat.args.map(a => DatalogScala.Var(a.name)))

          case SomePattern(v) =>
            DatalogScala.Eq(DatalogScala.Var(v.name), matcheeTerm)

          case NonePattern() =>
            DatalogScala.Undef(matcheeTerm)

          case _ => throw new IllegalStateException(s"Unknown pattern $pat")
        }
        patCons.addHint(SourceConstruct.from(exp -> pat))
        (bodyTerms, matcheeCons ++ (patCons +: bodyCons))
      }

    case BaseLit(code) =>
      import scala.meta._
      val evalOut = DatalogScala.Var(gensym.fresh("lit"))
      val resType =
        exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))
      val funCode = q"() => ${code.tree}"
      val evalConstraint =
        DatalogScala.Computed(evalOut, DatalogScala.Evaluation(Seq(), transType(resType), Scala(funCode)))
          .addHint(SourceConstruct.from(exp))
      Seq((Seq(evalOut), Seq(evalConstraint)))

    case BaseApplyUnary(op, exp) =>
      val expParam = {
        val typ = exp.typ.getOrElse(
          throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $exp")
        )
        param"exp: ${typ.asScala}"
      }

      val unary = meta.Term.ApplyUnary(op.tree, meta.Term.Name("exp"))
      val funCode = q"($expParam) => $unary"
      val resType = exp.typ.getOrElse(
        throw new IllegalStateException("cannot compile untyped base infix application")
      )

      val expRes = transExp(exp)
      val evalOut = DatalogScala.Var(gensym.fresh("eval"))
      for ((Seq(expTerm), expCons) <- expRes) yield {
        val evalConstraint = DatalogScala.Computed(
          evalOut,
          DatalogScala.Evaluation(
            Seq(expTerm -> transType(exp.typ.get)),
            transType(resType),
            Scala(funCode)
          )
        )
        (Seq(evalOut), expCons ++ Seq(evalConstraint))
      }

    case BaseApplyMethod(recv, method, args) =>
      import scala.meta._

      val paramsTyped = (recv +: args.getOrElse(Seq())).zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(
          throw new IllegalStateException(
            s"Cannot compile call of ${recv.prettyprint("")}.$method with untyped argument/reciever $arg"
          )
        )
        val paramName = gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs = paramsTyped.map(p => Term.Name(p.name.value))
      val methodName = Term.Name(method.name)
      val funCode =
        if (args.isEmpty)
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}"
        else
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}(..${scalaArgs.tail})"
      val resType =
        exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))

      val recvRes = transExp(recv)
      val argRes = args.getOrElse(Seq()).map(e => transExp(e))
      val evalOut = DatalogScala.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(recvRes +: argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(recv +: args.getOrElse(Nil)).map {
          case (t :: Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) =>
            throw new IllegalArgumentException(
              s"Cannot pass tuple argument $arg to ${recv.prettyprint("")}.$method"
            )
        }
        val evalConstraint = DatalogScala.Computed(
          evalOut,
          DatalogScala.Evaluation(flatArgTerms, transType(resType), Scala(funCode))
        )
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApply(fun, args) =>
      import scala.meta._
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(
          throw new IllegalStateException(s"Cannot compile call to $fun with untyped argument $arg")
        )
        val paramName = gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs: List[meta.Term] = paramsTyped.map(p => Term.Name(p.name.value))
      val funCode = q"(..$paramsTyped) => ${fun.tree}(..$scalaArgs)"
      val resType =
        exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))

      val argRes = args.map(e => transExp(e))
      val evalOut = DatalogScala.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(args).map {
          case (Nil, arg) =>
            throw new IllegalArgumentException(s"Cannot pass empty argument $arg to $fun")
          case (t :: Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) =>
            throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to $fun")
        }
        val evalConstraint = DatalogScala.Computed(
          evalOut,
          DatalogScala.Evaluation(flatArgTerms, transType(resType), Scala(funCode))
        )
          .addHint(SourceConstruct.from(exp))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApplyInfix(left, op, right)
        if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(
          _.isInstanceOf[TSet]
        ) =>
      transExp(left) ++ transExp(right)

    case BaseApplyInfix(left, op, right)
        if op.tree.value == "&" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(
          _.isInstanceOf[TSet]
        ) =>
      val transLeft = transExp(left)
      val transRight = transExp(right)

      // create substitution: replace every bound variable in right with freshly generated variable to avoid unwanted nameclashes after merging constraints from left and right
      val boundNamesInRight = right.vars.keys.map(_.name).toSet -- right.freevars.map(_.name.name)
      val freshVarsInRight = boundNamesInRight.map { n => DatalogScala.Var(gensym.fresh(n)) }
      val boundVarsInRight = boundNamesInRight.map(DatalogScala.Var)
      val subst = Substitute.fromMap(boundVarsInRight.zip(freshVarsInRight).toMap)

      for {
        (leftTerms, leftCons) <- transLeft
        (rightTerms, rightCons) <- transRight
      } yield {
        // apply substitution created above
        val renamedRightTerms = rightTerms.map(subst.substTerm)
        val renamedRightCons = rightCons.map(subst.substAtom)

        // generate equality constraints to force that constraints of left and right have to hold (X intersect Y implemented as X AND Y)
        val eqTerms = leftTerms.zip(renamedRightTerms).map { case (l, r) => DatalogScala.Eq(l, r) }
        (leftTerms, leftCons ++ renamedRightCons ++ eqTerms)
      }

    case BaseApplyInfix(left, op, right) =>
      val leftParam = {
        val typ = left.typ.getOrElse(
          throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $left")
        )
        param"left: ${typ.asScala}"
      }
      val rightParam = {
        val typ = right.typ.getOrElse(
          throw new IllegalStateException(
            s"Cannot compile call to $op with untyped argument $right"
          )
        )
        param"right: ${typ.asScala}"
      }
      val funCode = q"($leftParam, $rightParam) => left ${op.tree} right"
      val resType = exp.typ.getOrElse(
        throw new IllegalStateException("cannot compile untyped base infix application")
      )

      val leftRes = transExp(left)
      val rightRes = transExp(right)
      val evalOut = DatalogScala.Var(gensym.fresh("eval"))
      for {
        (Seq(leftTerm), leftCons) <- leftRes
        (Seq(rightTerm), rightCons) <- rightRes
      } yield {
        val evalConstraint = DatalogScala.Computed(
          evalOut,
          DatalogScala.Evaluation(
            Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
            transType(resType),
            Scala(funCode)
          )
        )
          .addHint(SourceConstruct.from(exp))
        (Seq(evalOut), leftCons ++ rightCons ++ Seq(evalConstraint))
      }

    case NoneExp() =>
      Seq() // yields no results

    case SomeExp(e) =>
      transExp(e) // yields the results of e

    case SetExp(es) =>
      es.flatMap(e => transExp(e))

    case mem @ SetMember(tup, Var(dataName), neg) if mem.isTypeMember =>
      // this is a type member test
      for ((Seq(term), tupCons) <- transExp(tup))
        yield {
          val typeTest =
            DatalogScala.Call(dataName.name, Seq(term), neg = neg)
              .addHint(IgnoreCall)
              .addHint(SourceConstruct.from(mem))
          (Seq(DatalogScala.host.True), tupCons :+ typeTest)
        }

    case SetMember(tup, set, neg) =>
      if (neg) {
        val pat = generatePattern(set, "set")
        generatedPatterns += pat
        val freeArgs = set.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }.map(_._1)
        for ((tupTerms, tupCons) <- transExp(tup)) yield {
          val negCall = DatalogScala.Call(pat.name, freeArgs ++ tupTerms, neg = true)
            .addHint(SourceConstruct.from(exp))
          (Seq(DatalogScala.host.True), tupCons :+ negCall)
        }
      } else
        for {
          (tupTerms, tupCons) <- transExp(tup)
          (setTerms, setCons) <- transExp(set)
        } yield {
          val eqs = tupTerms.zip(setTerms).map(vt =>
            DatalogScala.Eq(vt._1, vt._2).addHint(SourceConstruct.from(exp)))
          (Seq(DatalogScala.host.True), tupCons ++ setCons ++ eqs)
        }

    case SetComprehension(build, predicates) =>
      val predRes = predicates.map(e => transExp(e))
      for {
        ps <- TupleOps.cartesianProduct(predRes)
        (buildTerms, buildCons) <- transExp(build)
      } yield {
        val (predBools, predCons) = ps.unzip
        val predTrue =
          predBools.flatten.map(b => DatalogScala.Eq(b, DatalogScala.host.True).addHint(SourceConstruct.from(exp)))
        (buildTerms, predCons.flatten ++ predTrue ++ buildCons)
      }

    case SetFold(_, init, op, set) =>
      val tdataTyp = set.typ match {
        case Some(TSet(td: TData)) => Some(td)
        case _ => None
      }

      val aggregandPat = tdataTyp match {
        case Some(td) =>
          val pat = generatePattern(set, "AggregateCollection")
          // patch the pattern to coalesce the aggregand values
          val inParams = pat.params.slice(0, pat.params.size - 1)
          val oldOutName = pat.params.last.name
          val newOutName = gensym.fresh("out")
          val newOutParam = DatalogScala.Param(newOutName, transDataType(td))
          val coalesceCon = DatalogScala.Call(
            td.name.name + COALESCED_SUFFIX,
            Seq(DatalogScala.Var(oldOutName), DatalogScala.Var(newOutName))
          )
          pat.copy(
            params = inParams :+ newOutParam,
            bodies = pat.bodies.map(b => DatalogScala.Body(b.atoms :+ coalesceCon))
          ).withHints(pat)
        case None =>
          generatePattern(set, "AggregateCollection")
      }

      generatedPatterns += aggregandPat

      val freeArgs = set.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }.map(_._1)
      val description = s"init=$init, op=$op"
      val agg = genScala.genAggregation(
        description,
        init,
        op,
        exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped fold $exp"))
      )
      val outvar = DatalogScala.Var(gensym.fresh("out"))
      val dataTyp = transDataType(exp.typ.get)
      val aggregation = DatalogScala.CustomAggregation(
        dataTyp,
        Some(description),
        Scala(agg),
        aggregandPat.name,
        freeArgs :+ outvar,
        freeArgs.size
      )
      val foldVar = DatalogScala.Var(gensym.fresh("fold"))
      val compCon = DatalogScala.Computed(foldVar, aggregation).addHint(SourceConstruct.from(exp))

      tdataTyp match {
        case Some(td) =>
          // uncoalesce the aggregate result
          val foldVarUncoalesced = DatalogScala.Var(gensym.fresh("fold"))
          val uncoalesce =
            DatalogScala.Call(td.name.name + UNCOALESCED_SUFFIX, Seq(foldVar, foldVarUncoalesced))
          Seq((Seq(foldVarUncoalesced), Seq(compCon, uncoalesce)))
        case None =>
          Seq((Seq(foldVar), Seq(compCon)))
      }
  }

  val COALESCED_SUFFIX = "$Coalesced"
  val UNCOALESCED_SUFFIX = "$Uncoalesced"

  private def transData(data: DataDef): Seq[DatalogScala.Pattern] = {
    val vis = transVis(data.vis)
    val typ = transType(TData(data.name).resolved(data))

    val constrBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      DatalogScala.Body(
        Seq(
          DatalogScala.Call(
            name.name,
            paramTypes.zipWithIndex.map(pix => DatalogScala.Var(s"_${pix._2}")) :+ DatalogScala.Var("out")
          ).addHint(IgnoreCall, FixedAdornment(paramTypes.map(_ => true) :+ false))
        )
      )
    }

    val outParam = DatalogScala.Param("out", typ)
    val constrEDBBodies = data.constrs.map { constr =>
      generateEDBBody(constr, outParam)
    }
    val dataPat = DatalogScala.Pattern(
      None,
      data.name.name,
      Seq(DatalogScala.Param("out", typ)),
      constrBodies ++ constrEDBBodies
    ).addHint(DataHints.DataType)
      .addHint(NoInputRelation)

    val dataTyp = DatalogScala.TData(data.name.name)
    val uriParam = DatalogScala.Param("uri", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val dataParam = DatalogScala.Param("data", dataTyp)
    val constrCoalescedBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      DatalogScala.Body(
        Seq(
          DatalogScala.Call(
            name.name + COALESCED_SUFFIX,
            Seq(DatalogScala.Var(uriParam.name), DatalogScala.Var(dataParam.name))
          )
        )
      )
    }
    val constrUncoalescedBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      DatalogScala.Body(
        Seq(
          DatalogScala.Call(
            name.name + UNCOALESCED_SUFFIX,
            Seq(DatalogScala.Var(dataParam.name), DatalogScala.Var(uriParam.name))
          )
        )
      )
    }

    val dataCoalescedPat = DatalogScala.Pattern(
      None,
      data.name.name + COALESCED_SUFFIX,
      Seq(uriParam, dataParam),
      constrCoalescedBodies
    )
      .addHint(NoInputRelation)
    val dataUncoalescedPat = DatalogScala.Pattern(
      None,
      data.name.name + UNCOALESCED_SUFFIX,
      Seq(dataParam, uriParam),
      constrUncoalescedBodies
    )

    val dataParentPat = generateParent(data)
    val constructorPats = data.constrs.flatMap(transDataConstructor(_, vis, data))

    dataPat +: dataCoalescedPat +: dataUncoalescedPat +: dataParentPat +: constructorPats
  }

  val GP_URI: DatalogScala.TScala = DatalogScala.TScala(Scala(typeOf[truechange.URI]))
  val oMockURI: meta.Term = symbolOf(MockURI)
  val tyMockURI: meta.Type = typeOf[MockURI]

  private def transDataConstructor(
                                    constr: DataConstructor,
                                    vis: Option[DatalogScala.Visibility],
                                    data: DataDef
    ): Seq[DatalogScala.Pattern] = {
    val constrPat = generateConstructor(constr, vis, data)
    val selectorPat = generateSelector(constr, vis, data)
    val constrCoalescedPat = generateConstructorCoalesced(constr, vis, data)
    val constrUncoalescedPat = generateConstructorUncoalesced(constr, vis, data)
    Seq(
      constrPat,
      selectorPat,
      constrCoalescedPat,
      constrUncoalescedPat
    )
  }

  private def generateConstructor(
                                   constr: DataConstructor,
                                   vis: Option[DatalogScala.Visibility],
                                   data: DataDef
    ): DatalogScala.Pattern = {
    import scala.meta._

    val params = constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
      DatalogScala.Param(s"_$ix", transType(typ))
    }
    val outParam = DatalogScala.Param("out", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))

    val constrScalaFun = Term.Function(
      params.map(p => Term.Param(Nil, Term.Name(p.name), Some(DatalogScala.host.typeAsScala(p.typ)), None)).toList,
      q"""$oMockURI(${constr.name.name}, Seq(..${params.map(p => Term.Name(p.name)).toList}))"""
    )
    val outVar = DatalogScala.Var(outParam.name)
    val constrIDBBody = DatalogScala.Body(
      Seq(
        DatalogScala.Computed(
          outVar,
          DatalogScala.Evaluation(
            params.map(p => DatalogScala.Var(p.name) -> p.typ),
            GP_URI.addHint(DataHints.DataTypeName(data.name.name)),
            Scala(constrScalaFun)
          )
        )
      )
    ).addHint(DataHints.IDBConstructor)

    val kidVars =
      for (k <- constr.paramTypes.indices)
        yield DatalogScala.Var(s"_$k")
    val kidCoalescedVars =
      for (k <- constr.paramTypes.indices)
        yield DatalogScala.Var(kidVars(k).name + COALESCED_SUFFIX)

    val dataVar = DatalogScala.Var("data")
    val queryUncoalesced = DatalogScala.Call(constr.name.name + UNCOALESCED_SUFFIX, Seq(dataVar, outVar))
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(Seq(true, false)))
    val queryUncoalescedKids =
      for (k <- constr.paramTypes.indices)
        yield {
          val paramTyp = constr.paramTypes(k)
          val kidVar = kidVars(k)
          val kidCoalescedVar = kidCoalescedVars(k)

          // bind kidCoalescedVar to data.kid
          val scalaDataParam = Term.Name(dataVar.name)
          val constrScalaFun =
            q"($scalaDataParam: ${Type.Name(constr.name.name)}) => ${Term.Select(scalaDataParam, Term.Name(kidVar.name))}"
          val extractKid = DatalogScala.Computed(
            kidCoalescedVar,
            DatalogScala.Evaluation(
              Seq(dataVar -> transDataType(TData(constr.name))),
              transDataType(paramTyp),
              Scala(constrScalaFun)
            )
          )

          val bindKid = paramTyp match {
            case TData(name) =>
              // uncoalesce kidCoalescedVar to kidVar
              DatalogScala.Call(name.name + UNCOALESCED_SUFFIX, Seq(kidCoalescedVar, kidVar))
                .addHint(MagicSetHints.IgnoreCall)
                .addHint(MagicSetHints.FixedAdornment(Seq(true, false)))
            case TAny | TNothing | _: TScala =>
              // set kidVar = kidCoalescedVar
              DatalogScala.Eq(kidVar, kidCoalescedVar)
            case _ => throw new UnsupportedOperationException
          }
          Seq(extractKid, bindKid)
        }
    val constrUncoalescedBody = DatalogScala.Body(
      queryUncoalesced +: queryUncoalescedKids.flatten
    ).addHint(MagicSetHints.NoInputRelation)

    val constrPat = DatalogScala.Pattern(
      vis,
      constr.name.name,
      params :+ outParam,
      Seq(constrIDBBody, constrUncoalescedBody)
    ).addHint(DataHints.Constructor)
    constrPat
  }

  private def generateConstructorCoalesced(
                                            constr: DataConstructor,
                                            vis: Option[DatalogScala.Visibility],
                                            data: DataDef
    ): DatalogScala.Pattern = {
    import scala.meta._

    val uriParam = DatalogScala.Param("uri", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val uriVar = DatalogScala.Var(uriParam.name)
    val dataType = DatalogScala.TData(constr.name.name)
    val dataParam = DatalogScala.Param("data", dataType)
    val dataVar = DatalogScala.Var(dataParam.name)

    val kidVars =
      for (k <- constr.paramTypes.indices)
        yield DatalogScala.Var(s"_$k")
    val kidCoalescedVars =
      for (k <- constr.paramTypes.indices)
        yield DatalogScala.Var(kidVars(k).name + COALESCED_SUFFIX)

    val queryConstructor = DatalogScala.Call(constr.name.name, kidVars :+ uriVar)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(kidVars.map(_ => true) :+ false))
    val queryKids =
      for (k <- constr.paramTypes.indices)
        yield constr.paramTypes(k) match {
          case TData(name) =>
            DatalogScala.Call(name.name + COALESCED_SUFFIX, Seq(kidVars(k), kidCoalescedVars(k)))
          case TAny | TNothing | _: TScala =>
            DatalogScala.Eq(kidVars(k), kidCoalescedVars(k))
          case _ => throw new UnsupportedOperationException
        }

    val scalaParams =
      for (k <- constr.paramTypes.indices)
        yield Term.Param(
          Nil,
          Term.Name(kidCoalescedVars(k).name),
          Some(DatalogScala.host.typeAsScala(transDataType(constr.paramTypes(k)))),
          None
        )
    val constrScalaFun = Term.Function(
      scalaParams.toList,
      q"""${Term.Name(constr.name.name)}(..${kidCoalescedVars.map(v =>
          Term.Name(v.name)).toList})"""
    )
    val evalParams =
      for (k <- constr.paramTypes.indices)
        yield kidCoalescedVars(k) -> transDataType(constr.paramTypes(k))
    val genOutData =
      DatalogScala.Computed(dataVar, DatalogScala.Evaluation(evalParams, dataType, Scala(constrScalaFun)))
    val body = DatalogScala.Body(
      queryConstructor +:
        queryKids :+
        genOutData
    ).addHint(MagicSetHints.NoInputRelation)

    val constrCoalescedPat =
      DatalogScala.Pattern(vis, constr.name.name + COALESCED_SUFFIX, Seq(uriParam, dataParam), Seq(body))
        .addHint(MagicSetHints.NoInputRelation)
    constrCoalescedPat
  }

  private def generateConstructorUncoalesced(
                                              constr: DataConstructor,
                                              vis: Option[DatalogScala.Visibility],
                                              data: DataDef
    ): DatalogScala.Pattern = {
    import scala.meta._

    val uriParam = DatalogScala.Param("uri", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val uriVar = DatalogScala.Var(uriParam.name)
    val dataType = DatalogScala.TData(constr.name.name)
    val dataParam = DatalogScala.Param("data", dataType)
    val dataVar = DatalogScala.Var(dataParam.name)

    def consumeData(ty: DatalogScala.Type, f: Term => Term): DatalogScala.Evaluation = {
      val scalaDataParam = Term.Name(dataParam.name)
      val t = f(scalaDataParam)
      val constrScalaFun = q"($scalaDataParam: ${DatalogScala.host.typeAsScala(dataType)}) => $t"
      DatalogScala.Evaluation(Seq(dataVar -> dataType), ty, Scala(constrScalaFun))
    }

    val kidVars =
      for (k <- constr.paramTypes.indices)
        yield DatalogScala.Var(s"_$k")

    val uncoalesceKids =
      for (k <- constr.paramTypes.indices)
        yield constr.paramTypes(k) match {
          case td @ TData(name) =>
            val v = kidVars(k)
            val ty = transDataType(td)
            Seq(
              DatalogScala.Computed(v, consumeData(ty, t => Term.Select(t, Term.Name(v.name)))),
              DatalogScala.Call(name.name + UNCOALESCED_SUFFIX, Seq(v, DatalogScala.Var("_")))
            )
          case TAny | TNothing | _: TScala =>
            Seq()
          case _ => throw new UnsupportedOperationException
        }

    val genURI = DatalogScala.Computed(
      uriVar,
      consumeData(
        GP_URI.addHint(DataHints.DataTypeName(data.name.name)),
        t => q"new $tyMockURI($t.toString, Seq())"
      )
    )
    val body = DatalogScala.Body(
      uncoalesceKids.flatten :+
        genURI
    )

    val constrUncoalescedPat = DatalogScala.Pattern(
      vis,
      constr.name.name + UNCOALESCED_SUFFIX,
      Seq(dataParam, uriParam),
      Seq(body)
    )
    constrUncoalescedPat
  }

  private def generateEDBBody(constr: DataConstructor, outParam: DatalogScala.Param): DatalogScala.Body = {
    val outVar = DatalogScala.Var(outParam.name)
    val constrType = DatalogScala.TNode(constr.name.name)
    DatalogScala.Body(
      DatalogScala.HasType(outVar, constrType) +:
        constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
          DatalogScala.Path(
            outVar,
            constrType,
            DatalogScala.NamedLink(constrType, s"_$ix"),
            DatalogScala.Var(s"_$ix"),
            transRuntimeType(typ)
          )
        }
    ).addHint(MagicSetHints.NoInputRelation)
  }

  private def generateSelector(
                                constr: DataConstructor,
                                vis: Option[DatalogScala.Visibility],
                                data: DataDef
    ): DatalogScala.Pattern = {
    val params = constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
      DatalogScala.Param(s"_$ix", transType(typ))
    }

    val outParam = DatalogScala.Param("out", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val constrEDBBody = generateEDBBody(constr, outParam)

    val selectorCons =
      DatalogScala.Call(constr.name.name, (params :+ outParam).map(p => DatalogScala.Var(p.name)))
        .addHint(MagicSetHints.IgnoreCall)
        .addHint(MagicSetHints.FixedAdornment(params.map(_ => true) :+ false))
    val selectorPat = DatalogScala.Pattern(
      vis,
      constr.selectorName,
      outParam +: params,
      Seq(DatalogScala.Body(Seq(selectorCons)), constrEDBBody))
      .addHint(MagicSetHints.NoInputRelation)
      .addHint(DataHints.Selector(constr.name.name))
    selectorPat
  }

  private def generateParent(data: DataDef): DatalogScala.Pattern = {
    val dataDefs = module.content.collect { case dd: DataDef => dd }
    // val dataDef = dataDefs.find { data => data.constrs.contains(constructor) }.getOrElse(throw new IllegalStateException(s"Could not find data definition of given constructor ${constructor.name}"))
    val dataTy = TData(data.name)

    val wrappingConstructors = dataDefs.flatMap { data =>
      data.constrs.filter { cotr =>
        cotr.paramTypes.contains(dataTy)
      }
    }

    val param = gensym.fresh("param")
    val outParam = gensym.fresh("out")

    val params =
      Seq(DatalogScala.Param(param, transDataType(dataTy)), DatalogScala.Param(outParam, DatalogScala.TAny))
    val bodies = wrappingConstructors.flatMap { cotr =>
      cotr.paramTypes.zipWithIndex.filter(_._1 == dataTy).map { case (_, ix) =>
        val args = DatalogScala.Var(outParam) +: cotr.paramTypes.zipWithIndex.map { case (_, ix2) =>
          if (ix == ix2) DatalogScala.Var(param)
          else DatalogScala.Var("_")
        }
        val selectorCall = DatalogScala.Call(cotr.selectorName, args)
          .addHint(MagicSetHints.IgnoreCall)
          .addHint(MagicSetHints.FixedAdornment(params.map(_ => true) :+ false))
        DatalogScala.Body(Seq(selectorCall))
      }
    }
    DatalogScala.Pattern(None, data.parentName, params, bodies).addHint(MagicSetHints.NoInputRelation)
  }

  private def transVis(vis: Option[Visibility]): Option[DatalogScala.Visibility] =
    vis.map { case Private => DatalogScala.Private }

  @tailrec
  private def transType(typ: Type): DatalogScala.Type = typ match {
    case TAny => DatalogScala.TAny
    case TData(name) => GP_URI.addHint(DataHints.DataTypeName(name.name))
    case TScala(ty) => DatalogScala.TScala(ty)
    case TOption(ty) => transType(ty)
    case TSet(ty) => transType(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }

  private def transDataType(typ: Type): DatalogScala.Type = typ match {
    case TData(name) => DatalogScala.TData(name.name)
    case TAny | TNothing | _: TScala => DatalogScala.TScala(Scala(typ.asScala))
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Scala type")
  }

  private def transRuntimeType(typ: Type): DatalogScala.Type = typ match {
    case TAny => DatalogScala.TAny
    case TData(name) => DatalogScala.TNode(name.name)
    case TScala(Scala(meta.Type.Name(ty))) =>
      ty match {
        case "String" => DatalogScala.TLiteral.String
        case "Int" => DatalogScala.TLiteral.Int
        case "Boolean" => DatalogScala.TLiteral.Bool
        case "Long" => DatalogScala.TLiteral.Long
        case "Double" => DatalogScala.TLiteral.Double
        case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
      }
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
