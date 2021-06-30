package inca.frontend.functional.lowering

import inca.backend.hints.MagicSetHints.{FixedAdornment, IgnoreCall, NoInputRelation}
import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.ir.IR
import inca.frontend.functional.core._
import inca.runtime.data.DataURI
import inca.util.Scala.{symbolOf, typeOf}
import inca.util.{Gensym, Scala, TupleOps}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object GenerateIR {
  def transformModule(module: Module): IR.Module =
    new GenerateIR(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[IR.Module] =
    modules.map(transformModule)
}

class GenerateIR(module: Module) {
  private val gensym: Gensym = new Gensym(Iterable.empty)
  private val genScala = new GenerateScala

  private val generatedPatterns = ListBuffer[IR.Pattern]()

  def transModule(): IR.Module = {
    val Module(name, imports, contents) = module
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedDefNames.map(_.name))

    contents.foreach {
      case fun: FunctionDef => generatedPatterns += transFun(fun)
      case data: DataDef =>
        generatedPatterns ++= transData(data)
        genScala.genDataDef(data)
    }

    IR.Module(
      name.name,
      imports.map(_.name.name),
      generatedPatterns.toList,
      genScala.generated.map(Scala.apply))
  }

  private def transFun(fun: FunctionDef): IR.Pattern = gensym.scoped {
    gensym.register(fun.vars.keys.map(_.name))

    val vis = transVis(fun.vis)
    val params = fun.params.flatMap(p => flattenParam(p.name.name, p.typ, genFresh = false))
    val outParams = flattenParam("out", fun.outType, genFresh = true)

    val bodies = for ((terms, cons) <- transExp(fun.body))
      yield IR.Body(cons ++ outParams.zip(terms).map(pt => IR.Eq(IR.Var(pt._1.name), pt._2)))

    val pat = IR.Pattern(vis, fun.name.name, params ++ outParams, bodies)
    if (fun.hasAnnotation(MainFunctionAnno.key))
      pat.addHint(MagicSetHints.Main(params.map(_ => true) ++ outParams.map(_ => false)))
    pat
  }

  private def flattenParam(name: String, typ: Type, genFresh: Boolean): Seq[IR.Param] = typ match {
    case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
      flattenParam(name + "_" + ix, ty, genFresh = true)
    }
    case TOption(ty) => flattenParam(name, ty, genFresh)
    case TSet(ty) => flattenParam(name, ty, genFresh)
    case TNothing => Seq()
    case _ =>
      val v = if (genFresh) gensym.fresh(name) else name
      Seq(IR.Param(v, transType(typ)))
  }

  type ExpRes = Seq[(Seq[IR.Term], Seq[IR.Atom])]

  def generatePattern(exp: Expression, basename: String): IR.Pattern = {
    val name = gensym.freshGlobal(basename)
    val vars = exp.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }
    val params = vars.map { case (v,ty) => IR.Param(v.name, ty) }
    val expTys = exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped expression $exp")).flatten
    val outParams = expTys.map(ty => IR.Param(gensym.fresh("out"), transType(ty)))

    val bodies = for ((terms, cons) <- transExp(exp))
      yield IR.Body(cons ++ outParams.zip(terms).map(pt => IR.Eq(IR.Var(pt._1.name), pt._2)))

    IR.Pattern(None, name, params ++ outParams, bodies)
  }

  private def flatVars(x: Name, ty: Type): Seq[(IR.Var, IR.Type)] = ty match {
    case TTuple(ts) =>
      ts.zipWithIndex.map { case (ty,ix) => IR.Var(x.name + "$_" + ix) -> transType(ty) }
    case ty =>
      Seq(IR.Var(x.name) -> transType(ty))
  }

  private def transExp(exp: Expression): ExpRes = exp match {
    case Var(name) =>
      Seq((flatVars(name, exp.typ.getOrElse(throw new IllegalArgumentException(s"Untyped expression $exp"))).map(_._1), Seq()))

    case Let(names, _, bound, body) =>
      val tys = bound.typ.get match {
        case TTuple(tys) => tys
        case ty => Seq(ty)
      }
      val vars  = names.zip(tys).flatMap {
        case (name, ty) => flatVars(name, ty).map(_._1)
      }
      for ((boundTerms, boundCons) <- transExp(bound);
           (bodyTerm, bodyCons) <- transExp(body))
        yield {
          val eqs = vars.zip(boundTerms).map(vt => IR.Eq(vt._1, vt._2))
          (bodyTerm, boundCons ++ eqs ++ bodyCons)
        }

    case If(cnd, thn, els) =>
      val condTrans = transExp(cnd)
      val thnTrans = transExp(thn)
      val elsTrans = transExp(els)
      val thnRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condTrans;
             (thnTerm, thnCons) <- thnTrans)
          yield (thnTerm, cndCons ++ Seq(IR.Eq(cndTerm, IR.True)) ++ thnCons)
      val elsRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condTrans;
             (elsTerm, elsCons) <- elsTrans)
          yield (elsTerm, cndCons ++ Seq(IR.Eq(cndTerm, IR.False)) ++ elsCons)
      thnRes ++ elsRes

    case call@Call(Var(name), args, transitive) =>
      val outvars = call.fun.typ match {
        case Some(TFun(_, outType)) => outType.flatten.map(_ => IR.Var(gensym.fresh("call")))
        case Some(outType) => Seq(IR.Var(gensym.fresh("call")))
        case None => throw new IllegalArgumentException(s"Untyped call $call")
      }
      val argRes = args.map(e => transExp(e))

      // create single call constraint when no arguments passed
      if (argRes.isEmpty)
        return Seq((outvars, Seq(IR.Call(name.name, outvars, transitive, neg = false))))

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (outvars, argCons.flatten ++ Seq(IR.Call(name.name, argTerms.flatten ++ outvars, transitive, neg = false)))
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
      for ((pat, body) <- cases;
           (bodyTerms, bodyCons) <- transExp(body);
           (Seq(matcheeTerm), matcheeCons) <- matcheeRes) yield {
        val patCons = pat match {
          case pat: ConstructorPattern =>
            val selector = pat.target match {
              case Some(constr: DataConstructor) => constr.selectorName
              case Some(target) => throw new IllegalStateException(s"Unknown constructor target $target")
              case None => throw new IllegalArgumentException(s"Cannot compile unresolved constructor pattern $pat")
            }
            IR.Call(selector, matcheeTerm +: pat.args.map(a => IR.Var(a.name)))

          case SomePattern(v) =>
            IR.Eq(IR.Var(v.name), matcheeTerm)

          case NonePattern() =>
            IR.Undef(matcheeTerm)

          case _ => throw new IllegalStateException(s"Unknown pattern $pat")
        }
        (bodyTerms, matcheeCons ++ (patCons +: bodyCons))
      }

    case BaseLit(code) =>
      import scala.meta._
      val evalOut = IR.Var(gensym.fresh("lit"))
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))
      val funCode = q"() => ${code.tree}"
      val evalConstraint = IR.Computed(evalOut, IR.Evaluation(Seq(), transType(resType), Scala(funCode)))
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
      val evalOut = IR.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(args).map {
          case (Nil, arg) => throw new IllegalArgumentException(s"Cannot pass empty argument $arg to $fun")
          case (t::Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to $fun")
        }
        val evalConstraint = IR.Computed(evalOut, IR.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApplyInfix(left, op,  right)
      if op.tree.value == "++" && left.typ.exists(_.isInstanceOf[TSet]) && right.typ.exists(_.isInstanceOf[TSet]) =>
      transExp(left) ++ transExp(right)

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
      val evalOut = IR.Var(gensym.fresh("eval"))
      for ((Seq(leftTerm), leftCons) <- leftRes;
           (Seq(rightTerm), rightCons) <- rightRes) yield {
        val evalConstraint = IR.Computed(evalOut,
          IR.Evaluation(Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
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
            IR.Call(dataName.name, Seq(term), neg = neg).addHint(IgnoreCall)
          (Seq(IR.True), tupCons :+ typeTest)
        }

    case SetMember(tup, set, neg) =>
      if (neg) {
        val pat = generatePattern(set, "set")
        generatedPatterns += pat
        val freeArgs = set.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }.map(_._1)
        for ((tupTerms, tupCons) <- transExp(tup)) yield {
          val negCall = IR.Call(pat.name, freeArgs ++ tupTerms, neg = true)
          (Seq(IR.True), tupCons :+ negCall)
        }
      } else
        for ((tupTerms, tupCons) <- transExp(tup);
             (setTerms, setCons) <- transExp(set))
          yield {
            val eqs = tupTerms.zip(setTerms).map(vt => IR.Eq(vt._1, vt._2))
            (Seq(IR.True), tupCons ++ setCons ++ eqs)
          }

    case SetComprehension(build, predicates) =>
      val predRes = predicates.map(e => transExp(e))
      for (ps <- TupleOps.cartesianProduct(predRes);
           (buildTerms, buildCons) <- transExp(build)) yield {
        val (predBools, predCons) = ps.unzip
        val predTrue = predBools.flatten.map(b => IR.Eq(b, IR.True))
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
          val newOutParam = IR.Param(newOutName, transDataType(td))
          val coalesceCon = IR.Call(td.name.name + COALESCED_SUFFIX, Seq(IR.Var(oldOutName), IR.Var(newOutName)))
          pat.copy(params = inParams :+ newOutParam, bodies = pat.bodies.map(b => IR.Body(b.atoms :+ coalesceCon)))
        case None =>
          generatePattern(set, "AggregateCollection")
      }

      generatedPatterns += aggregandPat

      val freeArgs = set.vars.toSeq.flatMap { case (v, ty) => flatVars(v, ty.get) }.map(_._1)
      val description = s"init=$init, op=$op"
      val agg = genScala.genAggregation(description, init, op, exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped fold $exp")))
      val outvar = IR.Var(gensym.fresh("out"))
      val dataTyp = transDataType(exp.typ.get)
      val aggregation = IR.CustomAggregation(dataTyp, Some(description), Scala(agg), aggregandPat.name, freeArgs :+ outvar, freeArgs.size)
      val foldVar = IR.Var(gensym.fresh("fold"))
      val compCon = IR.Computed(foldVar, aggregation)

      tdataTyp match {
        case Some(td) =>
          // uncoalesce the aggregate result
          val foldVarUncoalesced = IR.Var(gensym.fresh("fold"))
          val uncoalesce = IR.Call(td.name.name + UNCOALESCED_SUFFIX, Seq(foldVar, foldVarUncoalesced))
          Seq((Seq(foldVarUncoalesced), Seq(compCon, uncoalesce)))
        case None =>
          Seq((Seq(foldVar), Seq(compCon)))
      }
  }


  val COALESCED_SUFFIX = "$Coalesced"
  val UNCOALESCED_SUFFIX = "$Uncoalesced"

  private def transData(data: DataDef): Seq[IR.Pattern] = {
    val vis = transVis(data.vis)
    val typ = transType(TData(data.name).resolved(data))

    val constrBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      IR.Body(Seq(
        IR.Call(name.name, paramTypes.zipWithIndex.map(pix => IR.Var(s"_${pix._2}")) :+ IR.Var("out"))
          .addHint(IgnoreCall, FixedAdornment(paramTypes.map(_ => true) :+ false))
      ))
    }


    val outParam = IR.Param("out", typ)
    val constrEDBBodies = data.constrs.map { constr =>
      generateEDBBody(constr, outParam)
    }
    val dataPat = IR.Pattern(None, data.name.name, Seq(IR.Param("out", typ)), constrBodies ++ constrEDBBodies).addHint(DataHints.DataType)
      .addHint(NoInputRelation)

    val dataTyp = IR.TData(data.name.name)
    val uriParam = IR.Param("uri", GP_URI)
    val dataParam = IR.Param("data", dataTyp)
    val constrCoalescedBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      IR.Body(Seq(IR.Call(name.name + COALESCED_SUFFIX, Seq(IR.Var(uriParam.name), IR.Var(dataParam.name)))))
    }
    val constrUncoalescedBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      IR.Body(Seq(
        IR.Call(name.name + UNCOALESCED_SUFFIX, Seq(IR.Var(dataParam.name), IR.Var(uriParam.name)))
      ))
    }

    val dataCoalescedPat = IR.Pattern(None, data.name.name + COALESCED_SUFFIX, Seq(uriParam, dataParam), constrCoalescedBodies)
      .addHint(NoInputRelation)
    val dataUncoalescedPat = IR.Pattern(None, data.name.name + UNCOALESCED_SUFFIX, Seq(dataParam, uriParam), constrUncoalescedBodies)

    dataPat +: dataCoalescedPat +: dataUncoalescedPat +: data.constrs.flatMap(transDataConstructor(_, vis, data))
  }

  val GP_URI: IR.TScala = IR.TScala(Scala(typeOf[truechange.URI]))
  val tDataURI: meta.Term = symbolOf(DataURI)

  private def transDataConstructor(constr: DataConstructor, vis: Option[IR.Visibility], data: DataDef): Seq[IR.Pattern] = {
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

  private def generateConstructor(constr: DataConstructor, vis: Option[IR.Visibility], data: DataDef): IR.Pattern = {
    import scala.meta._

    val params = constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
      IR.Param(s"_$ix", transType(typ))
    }
    val outParam = IR.Param("out", GP_URI)

    val constrScalaFun = Term.Function(
      params.map(p => Term.Param(Nil, Term.Name(p.name), Some(p.typ.asScala), None)).toList,
      q"""$tDataURI(${constr.name.name}, ..${params.map(p => Term.Name(p.name)).toList})"""
    )
    val outVar = IR.Var(outParam.name)
    val constrIDBBody = IR.Body(Seq(IR.Computed(outVar,
      IR.Evaluation(params.map(p => IR.Var(p.name) -> p.typ), GP_URI, Scala(constrScalaFun))))
    ).addHint(DataHints.IDBConstructor)

    val constrType = IR.TNode(constr.name.name)
//    val constrEDBBody = Datalog.Body(
////      Datalog.ExtensionalCall(constrType.name, Seq(outVar)) +:
//      Datalog.HasType(outVar, constrType) +:
//      constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
//        Datalog.Path(outVar, constrType, Datalog.NamedLink(constrType, s"_$ix"), Datalog.Var(s"_$ix"), transRuntimeType(typ))
//      }
//    ).addHint(MagicSetHints.NoInputRelation)


    val kidVars = for (k <- constr.paramTypes.indices)
      yield IR.Var(s"_$k")
    val kidCoalescedVars = for (k <- constr.paramTypes.indices)
      yield IR.Var(kidVars(k).name + COALESCED_SUFFIX)

    val dataVar = IR.Var("data")
    val queryUncoalesced = IR.Call(constr.name.name + UNCOALESCED_SUFFIX, Seq(dataVar, outVar))
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
        val extractKid = IR.Computed(kidCoalescedVar,
          IR.Evaluation(Seq(dataVar -> transDataType(TData(constr.name))), transDataType(paramTyp), Scala(constrScalaFun)))

        val bindKid = paramTyp match {
          case TData(name) =>
            // uncoalesce kidCoalescedVar to kidVar
            IR.Call(name + UNCOALESCED_SUFFIX, Seq(kidCoalescedVar, kidVar))
              .addHint(MagicSetHints.IgnoreCall)
              .addHint(MagicSetHints.FixedAdornment(Seq(true, false)))
          case TAny | TNothing | _: TScala =>
            // set kidVar = kidCoalescedVar
            IR.Eq(kidVar, kidCoalescedVar)
          case _ => throw new UnsupportedOperationException
        }
        Seq(extractKid, bindKid)
      }
    val constrUncoalescedBody = IR.Body(queryUncoalesced +: queryUncoalescedKids.flatten).addHint(MagicSetHints.NoInputRelation)

    val constrPat = IR.Pattern(vis, constr.name.name, params :+ outParam,
      Seq(constrIDBBody, constrUncoalescedBody)
    ).addHint(DataHints.Constructor)
    constrPat
  }


  private def generateConstructorCoalesced(constr: DataConstructor, vis: Option[IR.Visibility], data: DataDef): IR.Pattern = {
    import scala.meta._

    val uriParam = IR.Param("uri", GP_URI)
    val uriVar = IR.Var(uriParam.name)
    val dataType = IR.TData(constr.name.name)
    val dataParam = IR.Param("data", dataType)
    val dataVar = IR.Var(dataParam.name)

    val kidVars = for (k <- constr.paramTypes.indices)
      yield IR.Var(s"_$k")
    val kidCoalescedVars = for (k <- constr.paramTypes.indices)
      yield IR.Var(kidVars(k).name + COALESCED_SUFFIX)

    val queryConstructor = IR.Call(constr.name.name, kidVars :+ uriVar)
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(kidVars.map(_ => true) :+ false))
    val queryKids = for (k <- constr.paramTypes.indices)
      yield constr.paramTypes(k) match {
        case TData(name) =>
          IR.Call(name + COALESCED_SUFFIX, Seq(kidVars(k), kidCoalescedVars(k)))
        case TAny | TNothing | _: TScala =>
          IR.Eq(kidVars(k), kidCoalescedVars(k))
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
    val genOutData = IR.Computed(dataVar, IR.Evaluation(evalParams, dataType, Scala(constrScalaFun)))
    val body = IR.Body(
      queryConstructor +:
      queryKids :+
      genOutData
    ).addHint(MagicSetHints.NoInputRelation)

    val constrCoalescedPat = IR.Pattern(vis, constr.name.name + COALESCED_SUFFIX, Seq(uriParam, dataParam), Seq(body))
      .addHint(MagicSetHints.NoInputRelation)
    constrCoalescedPat
  }

  private def generateConstructorUncoalesced(constr: DataConstructor, vis: Option[IR.Visibility], data: DataDef): IR.Pattern = {
    import scala.meta._

    val uriParam = IR.Param("uri", GP_URI)
    val uriVar = IR.Var(uriParam.name)
    val dataType = IR.TData(constr.name.name)
    val dataParam = IR.Param("data", dataType)
    val dataVar = IR.Var(dataParam.name)

    def consumeData(ty: IR.Type, f: Term => Term): IR.Evaluation = {
      val scalaDataParam = Term.Name(dataParam.name)
      val t = f(scalaDataParam)
      val constrScalaFun = q"($scalaDataParam: ${dataType.asScala}) => $t"
      IR.Evaluation(Seq(dataVar -> dataType), ty, Scala(constrScalaFun))
    }

    val kidVars = for (k <- constr.paramTypes.indices)
      yield IR.Var(s"_$k")

    val uncoalesceKids = for (k <- constr.paramTypes.indices)
      yield constr.paramTypes(k) match {
        case td@TData(name) =>
          val v = kidVars(k)
          val ty = transDataType(td)
          Seq(
            IR.Computed(v, consumeData(ty, t => Term.Select(t, Term.Name(v.name)))),
            IR.Call(name + UNCOALESCED_SUFFIX, Seq(v, IR.Var("_")))
          )
        case TAny | TNothing | _: TScala =>
          Seq()
        case _ => throw new UnsupportedOperationException
      }

    val genURI = IR.Computed(uriVar, consumeData(GP_URI, t => q"$t.uri"))
    val body = IR.Body(
      uncoalesceKids.flatten :+
      genURI
    )

    val constrUncoalescedPat = IR.Pattern(vis, constr.name.name + UNCOALESCED_SUFFIX, Seq(dataParam, uriParam), Seq(body))
    constrUncoalescedPat
  }

  private def generateEDBBody(constr: DataConstructor, outParam: IR.Param): IR.Body = {
    val outVar = IR.Var(outParam.name)
    val constrType = IR.TNode(constr.name.name)
    IR.Body(
      IR.HasType(outVar, constrType) +:
        constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
          IR.Path(outVar, constrType, IR.NamedLink(constrType, s"_$ix"), IR.Var(s"_$ix"), transRuntimeType(typ))
        }
    ).addHint(MagicSetHints.NoInputRelation)
  }

  private def generateSelector(constr: DataConstructor, vis: Option[IR.Visibility], data: DataDef): IR.Pattern = {
    val params = constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
      IR.Param(s"_$ix", transType(typ))
    }

    val outParam = IR.Param("out", GP_URI)
    val constrEDBBody = generateEDBBody(constr, outParam)

    val selectorCons = IR.Call(constr.name.name, (params :+ outParam).map(p => IR.Var(p.name)))
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(params.map(_ => true) :+ false))
    val selectorPat = IR.Pattern(vis, constr.selectorName, outParam +: params, Seq(IR.Body(Seq(selectorCons)), constrEDBBody))
      .addHint(MagicSetHints.NoInputRelation)
      .addHint(DataHints.Selector)
    selectorPat
  }

  private def transVis(vis: Option[Visibility]): Option[IR.Visibility] =
    vis.map { case Private => IR.Private }

  @tailrec
  private def transType(typ: Type): IR.Type = typ match {
    case TAny => IR.TAny
    case TData(_) => GP_URI
    case TScala(ty) => IR.TScala(ty)
    case TOption(ty) => transType(ty)
    case TSet(ty) => transType(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }

  private def transDataType(typ: Type): IR.Type = typ match {
    case TData(name) => IR.TData(name.name)
    case TAny | TNothing | _: TScala => IR.TScala(Scala(typ.asScala))
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Scala type")
  }

  private def transRuntimeType(typ: Type): IR.Type = typ match {
    case TAny => IR.TAny
    case TData(name) => IR.TNode(name.name)
    case TScala(Scala(meta.Type.Name(ty))) =>
      ty match {
        case "String" => IR.TLiteral.String
        case "Int" => IR.TLiteral.Int
        case "Boolean" => IR.TLiteral.Bool
        case "Long" => IR.TLiteral.Long
        case "Double" => IR.TLiteral.Double
        case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
      }
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
