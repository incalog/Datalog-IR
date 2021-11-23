package inca.frontend.functionalxsouffle.lowering


import inca.backend.hints.MagicSetHints.{FixedAdornment, IgnoreCall, NoInputRelation}
import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.ir.{Datalog, Substitute}
import inca.frontend.functional.core._
import inca.frontend.functional.lowering.GenerateScala
import inca.runtime.data.MockURI
import inca.util.Scala.{symbolOf, typeOf}
import inca.util.{Gensym, Scala, TupleOps}

import scala.::
import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable.ListBuffer
import scala.meta.quasiquotes._

object GenerateDatalog {
  def transformModule(module: Module, externalSignatures: Map[Name, Seq[Type]]): Datalog.Module =
    new GenerateDatalog(module, externalSignatures).transModule()

  def transformModules(modules: Seq[Module], externalSignatures: Map[Name, Seq[Type]]): Seq[Datalog.Module] =
    modules.map(transformModule(_, externalSignatures))
}

class GenerateDatalog(module: Module, externalSignatures: Map[Name, Seq[Type]]) {
  private val gensym: Gensym = new Gensym(Iterable.empty)
  private val genScala = new GenerateScala

  val negatableSignatures: Set[Name] = externalSignatures.keys.toSet ++ module.content.flatMap {
    case fd: FunctionDef if fd.hasAnnotation(NoDemandAnno.key) => Some(fd.name)
    case _ => None
  }

  private val generatedPatterns = ListBuffer[Datalog.Pattern]()

  def transModule(): Datalog.Module = {
    val Module(name, imports, contents) = module
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedDefNames.map(_.name))

    contents.foreach {
      case fun: FunctionDef => generatedPatterns += transFun(fun)
      case data: DataDef =>
        generatedPatterns ++= transData(data)
        genScala.genDataDef(data)
    }

    Datalog.Module(
      name.name,
      imports.map(_.name.name),
      generatedPatterns.toList,
      genScala.generated.map(Scala.apply))
  }

  // needs to be reset before flattening params
  private var tupleVars: Map[Datalog.Name, Seq[Datalog.Name]] = Map()

  private def transFun(fun: FunctionDef): Datalog.Pattern = gensym.scoped {
    gensym.register(fun.vars.keys.map(_.name))

    val vis = transVis(fun.vis)
    // reset before flattening params
    tupleVars = Map()
    val params = fun.params.flatMap { p =>
      val res = flattenParam(p.name.name, p.typ, genFresh = false)
      if (res.size > 1) {
        tupleVars = tupleVars + (p.name.name -> res.map(_.name))
      }
      res
    }
    val outParams = flattenParam("out", fun.outType, genFresh = true)

    val bodies = for ((terms, cons) <- transExp(fun.body)(false))
      yield Datalog.Body(cons ++ outParams.zip(terms).map(pt => Datalog.Eq(Datalog.Var(pt._1.name), pt._2)))

    val pat = Datalog.Pattern(vis, fun.name.name, params ++ outParams, bodies)
    if (fun.hasAnnotation(MainFunctionAnno.key))
      pat.addHint(MagicSetHints.Main(params.map(_ => true) ++ outParams.map(_ => false)))
    if (fun.hasAnnotation(NoDemandAnno.key))
      pat.addHint(MagicSetHints.NoInputRelation)
    pat
  }

  private def flattenParam(name: String, typ: Type, genFresh: Boolean): Seq[Datalog.Param] = typ match {
    case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
      flattenParam(name + "_" + ix, ty, genFresh = true)
    }
    case TOption(ty) => flattenParam(name, ty, genFresh)
    case TSet(ty) => flattenParam(name, ty, genFresh)
    case TNothing => Seq()
    case _ =>
      val v = if (genFresh) gensym.fresh(name) else name
      Seq(Datalog.Param(v, transType(typ)))
  }

  type ExpRes = Seq[(Seq[Datalog.Term], Seq[Datalog.Atom])]

  def generatePattern(exp: Expression, basename: String): Datalog.Pattern = {
    val name = gensym.freshGlobal(basename)
    val vars = exp.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }
    val params = vars.map { case (v,ty) => Datalog.Param(v.name, ty) }
    val expTys = exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped expression $exp")).flatten
    val outParams = expTys.map(ty => Datalog.Param(gensym.fresh("out"), transType(ty)))

    val bodies = for ((terms, cons) <- transExp(exp)(false))
      yield Datalog.Body(cons ++ outParams.zip(terms).map(pt => Datalog.Eq(Datalog.Var(pt._1.name), pt._2)))

    Datalog.Pattern(None, name, params ++ outParams, bodies)
  }

  private def flatVars(x: Name, ty: Type): Seq[(Datalog.Var, Datalog.Type)] = ty match {
    case TTuple(ts) =>
      ts.zipWithIndex.map { case (ty,ix) => Datalog.Var(x.name + "$_" + ix) -> transType(ty) }
      tupleVars.get(x.name) match {
        case Some(vars) =>
          ts.zip(vars).map { case (ty,v) => Datalog.Var(v) -> transType(ty) }
        case None =>
          ts.zipWithIndex.map { case (ty,ix) => Datalog.Var(x.name + "$_" + ix) -> transType(ty) }
      }
    case ty =>
      Seq(Datalog.Var(x.name) -> transType(ty))
  }

  private def transExp(exp: Expression)(implicit inSetComprehension: Boolean): ExpRes = exp match {
    case Var(name) =>
      Seq((flatVars(name, exp.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $exp"))).map(_._1), Seq()))

    case Wildcard() =>
      val freshName = gensym.fresh("wildcard")
      Seq((flatVars(Name(freshName), exp.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $exp"))).map(_._1), Seq()))

    case Let(names, _, bound, body) =>
      val boundTy = bound.typ.get
      val tys = boundTy match {
        case TTuple(tys) => tys
        case ty => Seq(ty)
      }
      val vars =
        if (names.size == 1) {
          val res = flatVars(names.head, boundTy)
          tupleVars = tupleVars + (names.head.name -> res.map(_._1.name))
          res.map(_._1)
        } else {
          names.zip(tys).flatMap {
            case (name, ty) => flatVars(name, ty).map(_._1)
          }
        }
      for ((boundTerms, boundCons) <- transExp(bound);
           (bodyTerm, bodyCons) <- transExp(body))
      yield {
        val eqs = vars.zip(boundTerms).map(vt => Datalog.Eq(vt._1, vt._2))
        (bodyTerm, boundCons ++ eqs ++ bodyCons)
      }
    case TypeCast(e, ty) =>
      for ((Seq(eTerm), eCons) <- transExp(e)) yield {
        val instanceCall = Datalog.Call(ty.toString, Seq(eTerm))
        (Seq(eTerm), eCons :+ instanceCall)
      }

    case If(cnd, thn, els) =>
      val condTrans = transExp(cnd)
      val thnTrans = transExp(thn)
      val elsTrans = transExp(els)
      val thnRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condTrans;
             (thnTerm, thnCons) <- thnTrans)
        yield (thnTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.True)) ++ thnCons)
      val elsRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condTrans;
             (elsTerm, elsCons) <- elsTrans)
        yield (elsTerm, cndCons ++ Seq(Datalog.Eq(cndTerm, Datalog.False)) ++ elsCons)
      thnRes ++ elsRes

    case call@Call(Var(name), args, transitive) =>
      if (name.name == "parent") {
        transParentCall(call)
      } else if (name.name == "count") {
        transCountCall(call)
      } else {
        val outvars = call.fun.typ match {
          case Some(TFun(_, outType)) => outType.flatten.map(_ => Datalog.Var(gensym.fresh("call")))
          case Some(outType) => Seq(Datalog.Var(gensym.fresh("call")))
          case None => throw new IllegalArgumentException(s"Untyped call $call")
        }
        val argRes = args.map(e => transExp(e))

        // create single call constraint when no arguments passed
        if (argRes.isEmpty)
          return Seq((outvars, Seq(Datalog.Call(name.name, outvars, transitive, neg = false))))

        for (tups <- TupleOps.cartesianProduct(argRes)) yield {
          val (argTerms, argCons) = tups.unzip
          (outvars, argCons.flatten ++ Seq(Datalog.Call(name.name, argTerms.flatten ++ outvars, transitive, neg = false)))
        }
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
      val tys = matchee.typ.get match {
        case TTuple(tys) => tys
        case ty => Seq(ty)
      }
      val leftCombinations = missingPatterns(tys, cases)
      val defaultGuard = generateDefaultGuardPattern(tys, leftCombinations)
      generatedPatterns += defaultGuard
      for ((pat, body) <- cases;
           (bodyTerms, bodyCons) <- transExp(body);
           (matcheeTerms, matcheeCons) <- matcheeRes) yield {
          val patCons = transPattern(matcheeTerms, pat, defaultGuard)

        (bodyTerms, matcheeCons ++ patCons ++ bodyCons)
      }


      // figure out lift of typles, if no tuple we create singleton list
      // is there a valid wildcard pattern? if so generate defaultGuard dl pattern
      // compile each
      // add default case when there is wildcard pattern
      //matchee.typ.get match {
      //  case TTuple(ts) if cases.size > 1 =>
      //    val constructors = ts.map {
      //      case tdata: TData =>
      //        tdata.target.get.asInstanceOf[DataDef].constrs
      //      case _ => Seq()
      //    }

      //    val seenCombinations = collectSeenPatternCombinations(ts, cases)
      //    val constructorCombinations = TupleOps.cartesianProduct(constructors)
      //    // collect seen combinations
      //    val leftCombinations = constructorCombinations.filter { comb =>
      //      val combNames = comb.map(_.name)
      //      !seenCombinations.contains(combNames)
      //    }
      //    // generate relation enumerating combinations not seen
      //    val defaultGuardPattern = generateDefaultGuardPattern(ts.map(transType), leftCombinations)
      //    val defaultCase = cases.find { case (pat, _) =>
      //      pat match {
      //        case TuplePattern(pats) => pats.forall(p => p.isInstanceOf[WildcardPattern] || p.isInstanceOf[VarPattern])
      //        case WildcardPattern() => true
      //        case _ => false
      //      }
      //    }
      //    // TODO continue
      //    defaultCase match {
      //      case Some(_) if leftCombinations.isEmpty =>
      //        throw new IllegalArgumentException("The default case of the exhaustive pattern match will never execute")
      //      case Some(default) =>
      //        generatedPatterns += defaultGuardPattern
      //        val res = transExp(default._2)
      //        val defaultGuardCall = Datalog.Call(defaultGuardPattern.name, default._1.vars.map(v => Datalog.Var(v._1.name)).toSeq, transitive = false, neg = false)
      //        val x = for ((_, matcheeCons) <- matcheeRes) yield {
      //          res.map { case (terms, cons) => (terms, (matcheeCons :+ defaultGuardCall) ++ cons) }
      //        }

      //        x

      //      case None if leftCombinations.nonEmpty =>
      //        throw new IllegalArgumentException("The pattern matching expression is not exhaustive")
      //      case None =>
      //        // TODO normal exhaustive pattern match
      //    }

      //    Seq()
      //  case _ =>
      //    for ((pat, body) <- cases;
      //         (bodyTerms, bodyCons) <- transExp(body);
      //         (matcheeTerms, matcheeCons) <- matcheeRes) yield {
      //      matcheeTerms match {
      //        case Nil => throw new IllegalStateException("Matchee terms cannot be empty for pattern match")
      //        case Seq(matcheeTerm) =>
      //          val patCons = pat match {
      //            case pat: ConstructorPattern =>
      //              val selector = pat.target match {
      //                case Some(constr: DataConstructor) => constr.selectorName
      //                case Some(target) => throw new IllegalStateException(s"Unknown constructor target $target")
      //                case None => throw new IllegalArgumentException(s"Cannot compile unresolved constructor pattern $pat")
      //              }
      //              val patArgs = pat.args.map {
      //                case VarPattern(name) => Datalog.Var(name.name)
      //                case _ => throw new IllegalStateException(s"Non-variable nested in constructor pattern is not allowed in ${pat}")
      //              }
      //              Datalog.Call(selector, matcheeTerm +: patArgs)


      //            case SomePattern(VarPattern(name)) =>
      //              Datalog.Eq(Datalog.Var(name.name), matcheeTerm)
      //            case SomePattern(_) =>
      //              throw new IllegalArgumentException(s"Non-variable nested in some pattern is not allowed in ${pat}")

      //            case NonePattern() =>
      //              Datalog.Undef(matcheeTerm)

      //            case _ => throw new IllegalStateException(s"Unknown pattern $pat")
      //          }
      //          (bodyTerms, matcheeCons ++ (patCons +: bodyCons))
      //        case matcheeTerms => pat match {
      //          case TuplePattern(args) =>
      //            val patCons = args.zip(matcheeTerms).map {
      //              case (VarPattern(name), t) => Datalog.Eq(Datalog.Var(name.name), t)
      //              case _ => throw new IllegalStateException(s"Non-variable inside of tuple pattern in ${pat}")
      //            }
      //            (bodyTerms, matcheeCons ++ patCons ++ bodyCons)
      //          case _ => throw new IllegalStateException(s"Unknown pattern $pat")
      //        }
      //      }
      //    }
      //}

    case BaseLit(code) =>
      import scala.meta._
      val evalOut = Datalog.Var(gensym.fresh("lit"))
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))
      val funCode = q"() => ${code.tree}"
      val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(Seq(), transType(resType), Scala(funCode)))
      Seq((Seq(evalOut), Seq(evalConstraint)))

    case BaseApply(fun, args) =>
      import scala.meta._
      val paramsTyped = args.zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $fun with untyped argument $arg"))
        val paramName =  gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs: List[meta.Term] = paramsTyped.map(p => Term.Name(p.name.value))
      val funCode = q"(..$paramsTyped) => ${fun.tree}(..$scalaArgs)"
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))

      val argRes = args.map(e => transExp(e))
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(args).map {
          case (Nil, arg) => throw new IllegalArgumentException(s"Cannot pass empty argument $arg to $fun")
          case (t::Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to $fun")
        }
        val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApplyMethod(recv, method, args) =>
      import scala.meta._

      val paramsTyped = (recv +: args.getOrElse(Seq())).zipWithIndex.map { case (arg, ix) =>
        val argTyp = arg.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call of ${recv.prettyprint("")}.$method with untyped argument/reciever $arg"))
        val paramName =  gensym.fresh(s"arg$ix")
        param"${Term.Name(paramName)}: ${argTyp.asScala}"
      }.toList
      val scalaArgs: List[meta.Term] = paramsTyped.map(p => Term.Name(p.name.value))
      val methodName = Term.Name(method.name)
      val funCode =
        if (args.isEmpty)
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}"
        else
          q"(..$paramsTyped) => ${scalaArgs.head}.${methodName}(..${scalaArgs.tail})"
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))

      val recvRes = transExp(recv)
      val argRes = args.getOrElse(Seq()).map(e => transExp(e))
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(recvRes +: argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(recv +: args.getOrElse(Nil)).map {
          case (t::Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to ${recv.prettyprint("")}.$method")
        }
        val evalConstraint = Datalog.Computed(evalOut, Datalog.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApplyInfix(left, op,  right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      transExp(left) ++ transExp(right)

    case BaseApplyInfix(left, op,  right)
      if op.tree.value == "&" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      val transLeft = transExp(left)
      val transRight = transExp(right)

      // create substitution: replace every bound variable in right with freshly generated variable to avoid unwanted nameclashes after merging constraints from left and right
      val boundNamesInRight = right.vars.keys.map(_.name).toSet -- right.freevars.map(_.name.name)
      val freshVarsInRight = boundNamesInRight.map { n => Datalog.Var(gensym.fresh(n)) }
      val boundVarsInRight = boundNamesInRight.map(Datalog.Var)
      val subst = Substitute.fromMap(boundVarsInRight.zip(freshVarsInRight).toMap)

      for ((leftTerms, leftCons) <- transLeft;
           (rightTerms, rightCons) <- transRight) yield {
        // apply substitution created above
        val renamedRightTerms = rightTerms.map(subst.substTerm)
        val renamedRightCons = rightCons.map(subst.substAtom)

        // generate equality constraints to force that constraints of left and right have to hold (X intersect Y implemented as X AND Y)
        val eqTerms = leftTerms.zip(renamedRightTerms).map { case (l, r) => Datalog.Eq(l, r)}
        (leftTerms, leftCons ++ renamedRightCons ++ eqTerms)
      }




    case BaseApplyInfix(left, op, right) =>

      val leftParam = {
        val typ = left.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $left"))
        param"left: ${typ.asScala}"
      }
      val rightParam = {
        val typ = right.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $right"))
        param"right: ${typ.asScala}"
      }
      val funCode = q"($leftParam, $rightParam) => left ${op.tree} right"
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped base infix application"))

      val leftRes = transExp(left)
      val rightRes = transExp(right)
      val evalOut = Datalog.Var(gensym.fresh("eval"))
      for ((Seq(leftTerm), leftCons) <- leftRes;
           (Seq(rightTerm), rightCons) <- rightRes) yield {
        val evalConstraint = Datalog.Computed(evalOut,
          Datalog.Evaluation(Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
            transType(resType), Scala(funCode)))
        (Seq(evalOut), leftCons ++ rightCons ++ Seq(evalConstraint))
      }

    case NoneExp() =>
      Seq() // yields no results

    case SomeExp(e) =>
      transExp(e) // yields the results of e

    case SetExp(es) =>
      es.flatMap(e => transExp(e))

    case mem@SetMember(tup, Var(dataName), neg) if mem.isTypeMember =>
      // this is a type member test
      for ((Seq(term), tupCons) <- transExp(tup))
        yield {
          val typeTest =
            Datalog.Call(dataName.name, Seq(term), neg = neg).addHint(IgnoreCall)
          (Seq(Datalog.True), tupCons :+ typeTest)
        }

    case mem@SetMember(tup, Var(relName), neg) if negatableSignatures.contains(relName)=>
      transNegatableSetMember(relName, tup, neg)

    case SetMember(tup, Call(Var(name), args, _), neg) if negatableSignatures.contains(name) =>
      if (args.nonEmpty) {
        throw new IllegalArgumentException("Currently we do not support no-demand annotated functions with arguments")
      }
      transNegatableSetMember(name, tup, neg)

    case SetMember(tup, set, neg) =>
      if (neg) {
        val pat = generatePattern(set, "set")
        generatedPatterns += pat
        val freeArgs = set.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }.map(_._1)
        for ((tupTerms, tupCons) <- transExp(tup)) yield {
          val negCall = Datalog.Call(pat.name, freeArgs ++ tupTerms, neg = true)
          (Seq(Datalog.True), tupCons :+ negCall)
        }
      } else
        for ((tupTerms, tupCons) <- transExp(tup);
             (setTerms, setCons) <- transExp(set))
        yield {
          val eqs = tupTerms.zip(setTerms).map(vt => Datalog.Eq(vt._1, vt._2))
          (Seq(Datalog.True), tupCons ++ setCons ++ eqs)
        }

    case SetComprehension(build, predicates) =>
      val predRes = predicates.map(e => transExp(e)(true))
      for (ps <- TupleOps.cartesianProduct(predRes);
           (buildTerms, buildCons) <- transExp(build)) yield {
        val (predBools, predCons) = ps.unzip
        val predTrue = predBools.flatten.map(b => Datalog.Eq(b, Datalog.True))
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
          val newOutParam = Datalog.Param(newOutName, transDataType(td))
          val coalesceCon = Datalog.Call(td.name.name + COALESCED_SUFFIX, Seq(Datalog.Var(oldOutName), Datalog.Var(newOutName)))
          pat.copy(params = inParams :+ newOutParam, bodies = pat.bodies.map(b => Datalog.Body(b.atoms :+ coalesceCon)))
        case None =>
          generatePattern(set, "AggregateCollection")
      }

      generatedPatterns += aggregandPat

      val freeArgs = set.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }.map(_._1)
      val description = s"init=$init, op=$op"
      val agg = genScala.genAggregation(description, init, op, exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped fold $exp")))
      val outvar = Datalog.Var(gensym.fresh("out"))
      val dataTyp = transDataType(exp.typ.get)
      val aggregation = Datalog.CustomAggregation(dataTyp, Some(description), Scala(agg), aggregandPat.name, freeArgs :+ outvar, freeArgs.size)
      val foldVar = Datalog.Var(gensym.fresh("fold"))
      val compCon = Datalog.Computed(foldVar, aggregation)

      tdataTyp match {
        case Some(td) =>
          // uncoalesce the aggregate result
          val foldVarUncoalesced = Datalog.Var(gensym.fresh("fold"))
          val uncoalesce = Datalog.Call(td.name.name + UNCOALESCED_SUFFIX, Seq(foldVar, foldVarUncoalesced))
          Seq((Seq(foldVarUncoalesced), Seq(compCon, uncoalesce)))
        case None =>
          Seq((Seq(foldVar), Seq(compCon)))
      }
  }

  private def transNegatableSetMember(name: Name, tup: Expression, neg: Boolean)(implicit inSetComprehension: Boolean): ExpRes = {
    // we access an no demand relation
    // accessing an no demand relation negatively does not introduce a negated cycle because we do not generate an input relation for the relation
    val trueCase = for ((tupTerms, tupCons) <- transExp(tup)) yield {
      val call = Datalog.Call(name.name, tupTerms, transitive = false, neg).addHint(MagicSetHints.IgnoreCall)
      (Seq(Datalog.True), tupCons :+ call)
    }
    val falseCase = for ((tupTerms, tupCons) <- transExp(tup)) yield {
      val call = Datalog.Call(name.name, tupTerms, transitive = false, !neg).addHint(MagicSetHints.IgnoreCall)
      (Seq(Datalog.False), tupCons :+ call)
    }

    // we do not want to generate false case when set membership occurs as predicate in set comprehension
    if (inSetComprehension) trueCase
    else trueCase ++ falseCase
  }

  private def transParentCall(call: Call)(implicit inSetComprehension: Boolean): ExpRes = {
    val args = call.args
    for ((Seq(argTerm), argCons) <- transExp(args.head)) yield {
      // val argTy = transType(args.head.typ.get)
      val parentTerm = Datalog.Var(gensym.fresh("parent"))
      // val parentLinkCons = Datalog.Path(argTerm, argTy, Datalog.ParentLink, parentTerm, Datalog.TAny)
      // figure out what the dataDef is of the argument type
      val dataDef = args.head.typ.get match {
        case dty: TData => dty.target.get.asInstanceOf[DataDef]
        case _ => throw new IllegalStateException("Cannot happen")
      }
      val parentCall = Datalog.Call(dataDef.parentName, Seq(argTerm, parentTerm))
      (Seq(parentTerm), argCons :+ parentCall)
    }
  }

  private def transCountCall(call: Call)(implicit inSetComprehension: Boolean): ExpRes = {
    val args = call.args
    val (relName, callArgs) = args.head match {
      case Call(Var(name), callArgs, trans) => (name.name, callArgs)
      // case arg => new IllegalArgumentException(s"Expected call as argument of count but got ${arg}")
    }
    val argsRes = callArgs.map(e => transExp(e))

    for (tups <- TupleOps.cartesianProduct(argsRes)) yield {
      val (argTerms, argCons) = tups.unzip

      val countTerm = Datalog.Var(gensym.fresh("count"))
      val countAgg = Datalog.CountAggregation(relName, argTerms.flatten)
      val computed = Datalog.Computed(countTerm, countAgg)
      (Seq(countTerm), argCons.flatten ++ Seq(computed))
    }
  }

  private def missingPatterns(tys: Seq[Type], cases: Seq[(Pattern, Expression)]): Seq[Seq[DataConstructor]] = {
    val seenCombinations = collectSeenPatternCombinations(tys, cases)
    val constructors = tys.map {
      case tdata: TData =>
        tdata.target.get.asInstanceOf[DataDef].constrs
      case _ => Seq()
    }
    val constructorCombinations = TupleOps.cartesianProduct(constructors)
    // collect seen combinations
    constructorCombinations.filter { comb =>
      val combNames = comb.map(_.name)
      !seenCombinations.contains(combNames)
    }
  }

  private def transPattern(matcheeTerms: Seq[Datalog.Term], pattern: Pattern, defaultGuard: Datalog.Pattern): Seq[Datalog.Atom] = matcheeTerms match {
    case Nil => throw new IllegalStateException("Matchee terms cannot be empty for pattern match")
    case Seq(matcheeTerm) => pattern match {
      case cpat@ConstructorPattern(_, args) =>
        val selector = cpat.target match {
          case Some(constr: DataConstructor) => constr.selectorName
          case Some(target) => throw new IllegalStateException(s"Unknown constructor target $target")
          case None => throw new IllegalArgumentException(s"Cannot compile unresolved constructor pattern $pattern")
        }
        val patArgs = args.map {
          case VarPattern(name) => Datalog.Var(name.name)
          case _ => throw new IllegalStateException(s"Non-variable nested in constructor pattern is not allowed in ${pattern}")
        }
        Seq(Datalog.Call(selector, matcheeTerm +: patArgs))
      case VarPattern(name) =>
        Seq(Datalog.Eq(Datalog.Var(name.name), matcheeTerm))
      case WildcardPattern() =>
        throw new IllegalArgumentException("IMPLEMENT")
      case SomePattern(VarPattern(name)) =>
        Seq(Datalog.Eq(Datalog.Var(name.name), matcheeTerm))
      case NonePattern() =>
        Seq(Datalog.Undef(matcheeTerm))

      case _ => throw new IllegalArgumentException(s"Pattern $pattern not supported")
    }
    case _ => pattern match {
      case TuplePattern(pats) =>
        if (isDefaultPattern(pattern)) {
          Seq(Datalog.Call(defaultGuard.name, matcheeTerms, transitive = false, neg = false))
          // throw new IllegalArgumentException("IMPLEMENT DEFAULT CASE")
        } else {
          // case (x, y) => isDefaultPattern = true
          // case (_, _) => isDefaultPattern = true
          // case _ => isDefaultPattern = true
          // case x => isDefaultPattern = true
          // tup match {
          //   case (X(x1, x2), Y(y1, y2)) =>
          // }
          // tup_1
          // tup_2
          // unX(tup_1, x1, x2), unY(tup_2, y1, y2)

          if (onlyConstructorPatterns(pattern)) {
            // throw new IllegalArgumentException("NNONONONOON")
            matcheeTerms.zip(pats).map {
              case (t, cp@ConstructorPattern(cname, args)) =>
                val selArgs = args.map {
                  case VarPattern(v) => Datalog.Var(v.name)
                  case _ => throw new IllegalArgumentException("Non-variable patterns within constructor patterns not supported yet")
                }
                Datalog.Call(cp.selectorName, t +: selArgs, transitive = false, neg = false)
              case _ => throw new IllegalArgumentException("NOT SUPPORTED YEEEEET")
            }
          } else {
            throw new IllegalArgumentException("Non-constructor patterns within tuple patterns are not supported yet")
          }
          // vars
          // cotrs

          // ignore some and none are not supported
          //            val patCons = args.zip(matcheeTerms).map {
          //              case (VarPattern(name), t) => Datalog.Eq(Datalog.Var(name.name), t)
          //              case _ => throw new IllegalStateException(s"Non-variable inside of tuple pattern in ${pat}")
          //            }
          //            (bodyTerms, matcheeCons ++ patCons ++ bodyCons)

        }
        // check if the tuple pattern contains at least one constructor pattern
        // otherwise it is a
      case _ => throw new IllegalArgumentException(s"Pattern $pattern not supported")
    }
  }

  private def isDefaultPattern(p: Pattern): Boolean = p match {
    case _: VarPattern => true
    case _: WildcardPattern => true
    case TuplePattern(pats) => pats.forall(isDefaultPattern)
    case _ => false
  }

  private def containsConstructorPattern(p: Pattern): Boolean = p match {
    case ConstructorPattern(_, _) => true
    case TuplePattern(pats) => pats.exists(containsConstructorPattern)
    case _ => false
  }

  private def onlyConstructorPatterns(p: Pattern): Boolean = p match {
    case ConstructorPattern(_, _) => true
    case TuplePattern(pats) => pats.forall(onlyConstructorPatterns)
    case _ => false
  }

    //        case Nil => throw new IllegalStateException("Matchee terms cannot be empty for pattern match")
    //        case Seq(matcheeTerm) =>
    //          val patCons = pat match {
    //            case pat: ConstructorPattern =>
    //              val selector = pat.target match {
    //                case Some(constr: DataConstructor) => constr.selectorName
    //                case Some(target) => throw new IllegalStateException(s"Unknown constructor target $target")
    //                case None => throw new IllegalArgumentException(s"Cannot compile unresolved constructor pattern $pat")
    //              }
    //              val patArgs = pat.args.map {
    //                case VarPattern(name) => Datalog.Var(name.name)
    //                case _ => throw new IllegalStateException(s"Non-variable nested in constructor pattern is not allowed in ${pat}")
    //              }
    //              Datalog.Call(selector, matcheeTerm +: patArgs)


    //            case SomePattern(VarPattern(name)) =>
    //              Datalog.Eq(Datalog.Var(name.name), matcheeTerm)
    //            case SomePattern(_) =>
    //              throw new IllegalArgumentException(s"Non-variable nested in some pattern is not allowed in ${pat}")

    //            case NonePattern() =>
    //              Datalog.Undef(matcheeTerm)

    //            case _ => throw new IllegalStateException(s"Unknown pattern $pat")
    //          }
    //          (bodyTerms, matcheeCons ++ (patCons +: bodyCons))
    //        case matcheeTerms => pat match {
    //          case TuplePattern(args) =>
    //            val patCons = args.zip(matcheeTerms).map {
    //              case (VarPattern(name), t) => Datalog.Eq(Datalog.Var(name.name), t)
    //              case _ => throw new IllegalStateException(s"Non-variable inside of tuple pattern in ${pat}")
    //            }
    //            (bodyTerms, matcheeCons ++ patCons ++ bodyCons)
    //          case _ => throw new IllegalStateException(s"Unknown pattern $pat")
    //        }


  private def collectSeenPatternCombinations(ts: Seq[Type], cases: Seq[(Pattern, Expression)]): Seq[Seq[Name]] = {
    var seenCombinations = Seq[Seq[Name]]()
    cases.map(_._1).foreach {
      case TuplePattern(pats) =>
        if (pats.forall(_.isInstanceOf[ConstructorPattern])) {
          seenCombinations = seenCombinations :+ pats.map { case ConstructorPattern(name, _) => name }
        } else if (pats.forall(_.isInstanceOf[VarPattern])) {
          // this is the default pattern
        } else {
          throw new IllegalStateException("We currently do not allow mixed patterns in tuple patterns")
        }
      case _ => // do nothing
    }
    seenCombinations
  }


  private def generateDefaultGuardPattern(tupleTys: Seq[Type], combinations: Seq[Seq[DataConstructor]]): Datalog.Pattern = {
    val name = gensym.fresh("defaultGuard")
    val params = tupleTys.zipWithIndex.map { case (ty, idx) =>
      val paramName = gensym.fresh(s"arg$idx")
      Datalog.Param(paramName, transType(ty))
    }

    val bodies = combinations.map { constrs =>
      val selectorCalls = params.zip(constrs).map { case (param, constr) =>
        val subArgs = constr.paramTypes.map(_ => Datalog.Var(gensym.fresh("wildcard")))
        Datalog.Call(constr.selectorName, Datalog.Var(param.name) +: subArgs, transitive = false, neg = false)
      }
      Datalog.Body(selectorCalls)
    }
    Datalog.Pattern(None, name, params, bodies)
  }

  val COALESCED_SUFFIX = "$Coalesced"
  val UNCOALESCED_SUFFIX = "$Uncoalesced"



  private def transData(data: DataDef): Seq[Datalog.Pattern] = {
    val vis = transVis(data.vis)
    val typ = transType(TData(data.name).resolved(data))

    val constrBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      Datalog.Body(Seq(
        Datalog.Call(name.name, paramTypes.zipWithIndex.map(pix => Datalog.Var(s"_${pix._2}")) :+ Datalog.Var("out"))
          .addHint(IgnoreCall, FixedAdornment(paramTypes.map(_ => true) :+ false))
      ))
    }


    val outParam = Datalog.Param("out", typ)
    val constrEDBBodies = data.constrs.map { constr =>
      generateEDBBody(constr, outParam)
    }
    val dataPat = Datalog.Pattern(None, data.name.name, Seq(Datalog.Param("out", typ)), constrBodies ++ constrEDBBodies).addHint(DataHints.DataType)
      .addHint(NoInputRelation)

    val dataTyp = Datalog.TData(data.name.name)
    val uriParam = Datalog.Param("uri", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val dataParam = Datalog.Param("data", dataTyp)
    val constrCoalescedBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      Datalog.Body(Seq(Datalog.Call(name.name + COALESCED_SUFFIX, Seq(Datalog.Var(uriParam.name), Datalog.Var(dataParam.name)))))
    }
    val constrUncoalescedBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      Datalog.Body(Seq(
        Datalog.Call(name.name + UNCOALESCED_SUFFIX, Seq(Datalog.Var(dataParam.name), Datalog.Var(uriParam.name)))
      ))
    }

    val dataCoalescedPat = Datalog.Pattern(None, data.name.name + COALESCED_SUFFIX, Seq(uriParam, dataParam), constrCoalescedBodies)
      .addHint(NoInputRelation)
    val dataUncoalescedPat = Datalog.Pattern(None, data.name.name + UNCOALESCED_SUFFIX, Seq(dataParam, uriParam), constrUncoalescedBodies)

    val dataParentPat = generateParent(data)
    val constructorPats = data.constrs.flatMap(transDataConstructor(_, vis, data))

    dataPat +: dataCoalescedPat +: dataUncoalescedPat +: dataParentPat +: constructorPats
  }

  def GP_URI: Datalog.TScala = Datalog.TScala(Scala(typeOf[truechange.URI]))
  val oMockURI: meta.Term = symbolOf(MockURI)
  val tyMockURI: meta.Type = typeOf[MockURI]

  private def transDataConstructor(constr: DataConstructor, vis: Option[Datalog.Visibility], data: DataDef): Seq[Datalog.Pattern] = {
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

  private def generateConstructor(constr: DataConstructor, vis: Option[Datalog.Visibility], data: DataDef): Datalog.Pattern = {
    import scala.meta._

    val params = constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
      Datalog.Param(s"_$ix", transType(typ))
    }
    val outParam = Datalog.Param("out", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))

    val constrScalaFun = Term.Function(
      params.map(p => Term.Param(Nil, Term.Name(p.name), Some(p.typ.asScala), None)).toList,
      q"""$oMockURI(${constr.name.name}, ..${params.map(p => Term.Name(p.name)).toList})"""
    )
    val outVar = Datalog.Var(outParam.name)
    val constrIDBBody = Datalog.Body(Seq(Datalog.Computed(outVar,
      Datalog.Evaluation(params.map(p => Datalog.Var(p.name) -> p.typ), GP_URI.addHint(DataHints.DataTypeName(data.name.name)), Scala(constrScalaFun))))
    ).addHint(DataHints.IDBConstructor)

    val constrType = Datalog.TNode(constr.name.name)
    //    val constrEDBBody = Datalog.Body(
    ////      Datalog.ExtensionalCall(constrType.name, Seq(outVar)) +:
    //      Datalog.HasType(outVar, constrType) +:
    //      constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
    //        Datalog.Path(outVar, constrType, Datalog.NamedLink(constrType, s"_$ix"), Datalog.Var(s"_$ix"), transRuntimeType(typ))
    //      }
    //    ).addHint(MagicSetHints.NoInputRelation)


    val kidVars = for (k <- constr.paramTypes.indices)
      yield Datalog.Var(s"_$k")
    val kidCoalescedVars = for (k <- constr.paramTypes.indices)
      yield Datalog.Var(kidVars(k).name + COALESCED_SUFFIX)

    val dataVar = Datalog.Var("data")
    val queryUncoalesced = Datalog.Call(constr.name.name + UNCOALESCED_SUFFIX, Seq(dataVar, outVar))
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(Seq(true, false)))
    val queryUncoalescedKids = for (k <- constr.paramTypes.indices)
      yield {
        val paramTyp = constr.paramTypes(k)
        val kidVar = kidVars(k)
        val kidCoalescedVar = kidCoalescedVars(k)

        // bind kidCoalescedVar to data.kid
        val scalaDataParam = Term.Name(dataVar.name)
        val constrScalaFun = q"($scalaDataParam: ${Type.Name(constr.name.name)}) => ${Term.Select(scalaDataParam, Term.Name(kidVar.name))}"
        val extractKid = Datalog.Computed(kidCoalescedVar,
          Datalog.Evaluation(Seq(dataVar -> transDataType(TData(constr.name))), transDataType(paramTyp), Scala(constrScalaFun)))

        val bindKid = paramTyp match {
          case TData(name) =>
            // uncoalesce kidCoalescedVar to kidVar
            Datalog.Call(name + UNCOALESCED_SUFFIX, Seq(kidCoalescedVar, kidVar))
              .addHint(MagicSetHints.IgnoreCall)
              .addHint(MagicSetHints.FixedAdornment(Seq(true, false)))
          case TAny | TNothing | _: TScala =>
            // set kidVar = kidCoalescedVar
            Datalog.Eq(kidVar, kidCoalescedVar)
          case _ => throw new UnsupportedOperationException
        }
        Seq(extractKid, bindKid)
      }
    val constrUncoalescedBody = Datalog.Body(queryUncoalesced +: queryUncoalescedKids.flatten).addHint(MagicSetHints.NoInputRelation)

    val constrPat = Datalog.Pattern(vis, constr.name.name, params :+ outParam,
      Seq(constrIDBBody, constrUncoalescedBody)
    ).addHint(DataHints.Constructor)
    constrPat
  }


  private def generateConstructorCoalesced(constr: DataConstructor, vis: Option[Datalog.Visibility], data: DataDef): Datalog.Pattern = {
    import scala.meta._

    val uriParam = Datalog.Param("uri", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val uriVar = Datalog.Var(uriParam.name)
    val dataType = Datalog.TData(constr.name.name)
    val dataParam = Datalog.Param("data", dataType)
    val dataVar = Datalog.Var(dataParam.name)

    val kidVars = for (k <- constr.paramTypes.indices)
      yield Datalog.Var(s"_$k")
    val kidCoalescedVars = for (k <- constr.paramTypes.indices)
      yield Datalog.Var(kidVars(k).name + COALESCED_SUFFIX)

    val queryConstructor = Datalog.Call(constr.name.name, kidVars :+ uriVar)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(kidVars.map(_ => true) :+ false))
    val queryKids = for (k <- constr.paramTypes.indices)
      yield constr.paramTypes(k) match {
        case TData(name) =>
          Datalog.Call(name + COALESCED_SUFFIX, Seq(kidVars(k), kidCoalescedVars(k)))
        case TAny | TNothing | _: TScala =>
          Datalog.Eq(kidVars(k), kidCoalescedVars(k))
        case _ => throw new UnsupportedOperationException
      }

    val scalaParams = for (k <- constr.paramTypes.indices)
      yield Term.Param(Nil, Term.Name(kidCoalescedVars(k).name), Some(transDataType(constr.paramTypes(k)).asScala), None)
    val constrScalaFun = Term.Function(
      scalaParams.toList,
      q"""${Term.Name(constr.name.name)}(..${kidCoalescedVars.map(v => Term.Name(v.name)).toList})"""
    )
    val evalParams = for (k <- constr.paramTypes.indices)
      yield kidCoalescedVars(k) -> transDataType(constr.paramTypes(k))
    val genOutData = Datalog.Computed(dataVar, Datalog.Evaluation(evalParams, dataType, Scala(constrScalaFun)))
    val body = Datalog.Body(
      queryConstructor +:
        queryKids :+
        genOutData
    ).addHint(MagicSetHints.NoInputRelation)

    val constrCoalescedPat = Datalog.Pattern(vis, constr.name.name + COALESCED_SUFFIX, Seq(uriParam, dataParam), Seq(body))
      .addHint(MagicSetHints.NoInputRelation)
    constrCoalescedPat
  }

  private def generateConstructorUncoalesced(constr: DataConstructor, vis: Option[Datalog.Visibility], data: DataDef): Datalog.Pattern = {
    import scala.meta._

    val uriParam = Datalog.Param("uri", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val uriVar = Datalog.Var(uriParam.name)
    val dataType = Datalog.TData(constr.name.name)
    val dataParam = Datalog.Param("data", dataType)
    val dataVar = Datalog.Var(dataParam.name)

    def consumeData(ty: Datalog.Type, f: Term => Term): Datalog.Evaluation = {
      val scalaDataParam = Term.Name(dataParam.name)
      val t = f(scalaDataParam)
      val constrScalaFun = q"($scalaDataParam: ${dataType.asScala}) => $t"
      Datalog.Evaluation(Seq(dataVar -> dataType), ty, Scala(constrScalaFun))
    }

    val kidVars = for (k <- constr.paramTypes.indices)
      yield Datalog.Var(s"_$k")

    val uncoalesceKids = for (k <- constr.paramTypes.indices)
      yield constr.paramTypes(k) match {
        case td@TData(name) =>
          val v = kidVars(k)
          val ty = transDataType(td)
          Seq(
            Datalog.Computed(v, consumeData(ty, t => Term.Select(t, Term.Name(v.name)))),
            Datalog.Call(name + UNCOALESCED_SUFFIX, Seq(v, Datalog.Var("_")))
          )
        case TAny | TNothing | _: TScala =>
          Seq()
        case _ => throw new UnsupportedOperationException
      }

    val genURI = Datalog.Computed(
      uriVar,
      consumeData(GP_URI.addHint(DataHints.DataTypeName(data.name.name)), t => q"new $tyMockURI($t.toString)"))
    val body = Datalog.Body(
      uncoalesceKids.flatten :+
        genURI
    )

    val constrUncoalescedPat = Datalog.Pattern(vis, constr.name.name + UNCOALESCED_SUFFIX, Seq(dataParam, uriParam), Seq(body))
    constrUncoalescedPat
  }

  private def generateEDBBody(constr: DataConstructor, outParam: Datalog.Param): Datalog.Body = {
    val outVar = Datalog.Var(outParam.name)
    val constrType = Datalog.TNode(constr.name.name)
    Datalog.Body(
      Datalog.HasType(outVar, constrType) +:
        constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
          Datalog.Path(outVar, constrType, Datalog.NamedLink(constrType, s"_$ix"), Datalog.Var(s"_$ix"), transRuntimeType(typ))
        }
    ).addHint(MagicSetHints.NoInputRelation)
  }

  private def generateSelector(constr: DataConstructor, vis: Option[Datalog.Visibility], data: DataDef): Datalog.Pattern = {
    val params = constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
      Datalog.Param(s"_$ix", transType(typ))
    }

    val outParam = Datalog.Param("out", GP_URI.addHint(DataHints.DataTypeName(data.name.name)))
    val constrEDBBody = generateEDBBody(constr, outParam)

    val selectorCons = Datalog.Call(constr.name.name, (params :+ outParam).map(p => Datalog.Var(p.name)))
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(params.map(_ => true) :+ false))
    val selectorPat = Datalog.Pattern(vis, constr.selectorName, outParam +: params, Seq(Datalog.Body(Seq(selectorCons)), constrEDBBody))
      .addHint(MagicSetHints.NoInputRelation)
      .addHint(DataHints.Selector)
    selectorPat
  }

  private def generateParent(data: DataDef): Datalog.Pattern = {
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


    val params = Seq(Datalog.Param(param, transDataType(dataTy)), Datalog.Param(outParam, Datalog.TAny))
    val bodies = wrappingConstructors.flatMap { cotr =>
      cotr.paramTypes.zipWithIndex.filter(_._1 == dataTy).map { case (_, ix) =>
        val args = Datalog.Var(outParam) +: cotr.paramTypes.zipWithIndex.map { case (_, ix2) =>
          if (ix == ix2) Datalog.Var(param)
          else Datalog.Var("_")
        }
        val selectorCall = Datalog.Call(cotr.selectorName, args)
          .addHint(MagicSetHints.IgnoreCall)
          .addHint(MagicSetHints.FixedAdornment(params.map(_ => true) :+ false))
        Datalog.Body(Seq(selectorCall))
      }
    }
    Datalog.Pattern(None, data.parentName, params, bodies).addHint(MagicSetHints.NoInputRelation)
  }

  private def transVis(vis: Option[Visibility]): Option[Datalog.Visibility] =
    vis.map { case Private => Datalog.Private }

  @tailrec
  private def transType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TData(name) => GP_URI.addHint(DataHints.DataTypeName(name.name))
    case TScala(ty) => Datalog.TScala(ty)
    case TOption(ty) => transType(ty)
    case TSet(ty) => transType(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }

  private def transDataType(typ: Type): Datalog.Type = typ match {
    case TData(name) => Datalog.TData(name.name)
    case TAny | TNothing | _: TScala => Datalog.TScala(Scala(typ.asScala))
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Scala type")
  }

  private def transRuntimeType(typ: Type): Datalog.Type = typ match {
    case TAny => Datalog.TAny
    case TData(name) => Datalog.TNode(name.name)
    case TScala(Scala(meta.Type.Name(ty))) =>
      ty match {
        case "String" => Datalog.TLiteral.String
        case "Int" => Datalog.TLiteral.Int
        case "Boolean" => Datalog.TLiteral.Bool
        case "Long" => Datalog.TLiteral.Long
        case "Double" => Datalog.TLiteral.Double
        case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
      }
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
