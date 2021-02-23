package inca.frontend.lowering

import inca.backend.hints.{DataHints, MagicSetHints}
import inca.backend.ir.GP
import inca.frontend.core._
import inca.runtime.data.DataURI
import inca.util.Meta.{Scala, symbolOf, typeOf}
import inca.util.{Gensym, TupleOps}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object GenerateDatalog {
  def transformModule(module: Module): GP.Module =
    new GenerateDatalog(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[GP.Module] =
    modules.map(transformModule)
}

class GenerateDatalog(module: Module) {
  val gensym: Gensym = new Gensym(Iterable.empty)

  private val generatedPatterns = ListBuffer[GP.Pattern]()

  def transModule(): GP.Module = {
    val Module(name, imports, contents) = module
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedDefNames.map(_.name))

    val scalaModuleContents = ListBuffer[meta.Import]()
    val dataContents = ListBuffer[GP.DataDef]()
    contents.foreach {
      case fun: FunctionDef => generatedPatterns += transFun(fun)
      case data: DataDef =>
        generatedPatterns ++= transData(data)
        dataContents += lowerData(data)
    }

    val scalaContent = scalaModuleContents.toList
    GP.Module(
      name.name,
      imports.map(_.name.name),
      dataContents.toList,
      generatedPatterns.toList,
      scalaContent.map(Scala.apply))
  }

  private def transFun(fun: FunctionDef): GP.Pattern = gensym.scoped {
    gensym.register(fun.vars.keys.map(_.name))

    val vis = transVis(fun.vis)
    val params = fun.params.flatMap(p => flattenParam(p.name.name, p.typ, genFresh = false))
    val outParams = flattenParam("out", fun.outType, genFresh = true)

    val bodies = for ((terms, cons) <- transExp(fun.body.ensureCore))
      yield GP.Body(cons ++ outParams.zip(terms).map(pt => GP.Eq(GP.Var(pt._1.name), pt._2)))

    val pat = GP.Pattern(vis, fun.name.name, params ++ outParams, bodies)
    if (fun.hasAnnotation(MainFunctionAnno.key))
      pat.addHint(MagicSetHints.Main(params.map(_ => true) :+ false))
    pat
  }

  private def flattenParam(name: String, typ: Type, genFresh: Boolean): Seq[GP.Param] = typ match {
    case TTuple(tys) => tys.zipWithIndex.flatMap { case (ty, ix) =>
      flattenParam(name + "_" + ix, ty, genFresh = true)
    }
    case TNothing => Seq()
    case _ =>
      val v = if (genFresh) gensym.fresh(name) else name
      Seq(GP.Param(v, transType(typ)))
  }

  type ExpRes = Seq[(Seq[GP.Term], Seq[GP.Constraint])]

  private def transExp(exp: CoreExpression): ExpRes = exp match {
    case Var(name) =>
      val v = GP.Var(name.name)
      Seq((Seq(v), Seq()))

    case Let(names, _, bound, body) =>
      val vars  = names.map(name => GP.Var(name.name))
      for ((boundTerms, boundCons) <- transExp(bound.ensureCore);
           (bodyTerm, bodyCons) <- transExp(body.ensureCore))
        yield {
          val eqs = vars.zip(boundTerms).map(vt => GP.Eq(vt._1, vt._2))
          (bodyTerm, boundCons ++ eqs ++ bodyCons)
        }

    case If(cnd, thn, els) =>
      val condTrans = transExp(cnd.ensureCore)
      val thnTrans = transExp(thn.ensureCore)
      val elsTrans = transExp(els.ensureCore)
      val thnRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condTrans;
             (thnTerm, thnCons) <- thnTrans)
          yield (thnTerm, cndCons ++ Seq(GP.Eq(cndTerm, GP.True)) ++ thnCons)
      val elsRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condTrans;
             (elsTerm, elsCons) <- elsTrans)
          yield (elsTerm, cndCons ++ Seq(GP.Eq(cndTerm, GP.False)) ++ elsCons)
      thnRes ++ elsRes

    case call@Call(name, args, transitive) =>
      val outvars = call.target match {
        case Some(fun: FunctionDef) => fun.outParams.map(_ => GP.Var(gensym.fresh("out")))
        case Some(ctr: DataConstructor) => Seq(GP.Var(gensym.fresh("out_" + ctr.name.name)))
        case Some(target) => throw new IllegalArgumentException(s"Unknown call target $target")
        case None => throw new IllegalArgumentException(s"Unresolved call $call")
      }
      val argRes = args.map(e => transExp(e.ensureCore))

      // create single call constraint when no arguments passed
      if (argRes.isEmpty)
        return Seq((outvars, Seq(GP.Call(name.name, outvars, transitive, neg = false))))

      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTerms, argCons) = tups.unzip
        (outvars, argCons.flatten ++ Seq(GP.Call(name.name, argTerms.flatten ++ outvars, transitive, neg = false)))
      }

    case Tuple(exps) =>
      val expRes = exps.map(e => transExp(e.ensureCore))
      for (tups <- TupleOps.cartesianProduct(expRes)) yield {
        val (terms, cons) = tups.unzip
        (terms.flatten, cons.flatten)
      }

    case Match(matchee, cases) =>
      val matcheeRes = transExp(matchee.ensureCore)
      for ((pat, body) <- cases;
           (bodyTerms, bodyCons) <- transExp(body.ensureCore);
           (Seq(matcheeTerm), matcheeCons) <- matcheeRes) yield {
        val patCons = pat match {
          case pat: ConstructorPattern =>
            val selector = pat.target match {
              case Some(constr: DataConstructor) => constr.selectorName
              case Some(target) => throw new IllegalStateException(s"Unknown constructor target $target")
              case None => throw new IllegalArgumentException(s"Cannot compile unresolved constructor pattern $pat")
            }
            GP.Call(selector, matcheeTerm +: pat.args.map(a => GP.Var(a.name)))

          case SomePattern(v) =>
            GP.Eq(GP.Var(v.name), matcheeTerm)

          case NonePattern() =>
            GP.Undef(matcheeTerm)

          case _ => throw new IllegalStateException(s"Unknown pattern $pat")
        }
        (bodyTerms, matcheeCons ++ (patCons +: bodyCons))
      }

    case BaseLit(code) =>
      import scala.meta._
      val evalOut = GP.Var(gensym.fresh("lit"))
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))
      val funCode = q"() => ${code.tree}"
      val evalConstraint = GP.Computed(evalOut, GP.Evaluation(Seq(), transType(resType), Scala(funCode)))
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

      val argRes = args.map(e => transExp(e.ensureCore))
      val evalOut = GP.Var(gensym.fresh("eval"))
      for (tups <- TupleOps.cartesianProduct(argRes)) yield {
        val (argTermss, argCons) = tups.unzip
        val flatArgTerms = argTermss.zip(args).map {
          case (Nil, arg) => throw new IllegalArgumentException(s"Cannot pass empty argument $arg to $fun")
          case (t::Nil, arg) => (t, transType(arg.typ.get))
          case (_, arg) => throw new IllegalArgumentException(s"Cannot pass tuple argument $arg to $fun")
        }
        val evalConstraint = GP.Computed(evalOut, GP.Evaluation(flatArgTerms, transType(resType), Scala(funCode)))
        (Seq(evalOut), argCons.flatten :+ evalConstraint)
      }

    case BaseApplyInfix(left, op, right) =>
      import scala.meta._
      val leftParam = {
        val typ = left.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $left"))
        param"left: ${typ.asScala}"
      }
      val rightParam = {
        val typ = right.typ.getOrElse(throw new IllegalStateException(s"Cannot compile call to $op with untyped argument $right"))
        param"right: ${typ.asScala}"
      }
      val funCode = q"($leftParam, $rightParam) => left ${op.tree} right"
      val resType = exp.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))

      val leftRes = transExp(left.ensureCore)
      val rightRes = transExp(right.ensureCore)
      val evalOut = GP.Var(gensym.fresh("eval"))
      for ((Seq(leftTerm), leftCons) <- leftRes;
           (Seq(rightTerm), rightCons) <- rightRes) yield {
        val evalConstraint = GP.Computed(evalOut,
          GP.Evaluation(Seq(leftTerm -> transType(left.typ.get), rightTerm -> transType(right.typ.get)),
            transType(resType), Scala(funCode)))
        (Seq(evalOut), leftCons ++ rightCons ++ Seq(evalConstraint))
      }

    case NoneExp() =>
      Seq() // yields no results

    case SomeExp(e) =>
      transExp(e.ensureCore) // yields the results of e
  }

  private def transData(data: DataDef): Seq[GP.Pattern] = {
    val vis = transVis(data.vis)
    val typ = transType(TData(data.name).resolved(data))

    val constrBodies = data.constrs.map { case DataConstructor(name, paramTypes) =>
      GP.Body(Seq(
        GP.Call(name.name, paramTypes.zipWithIndex.map(pix => GP.Var(s"_${pix._2}")) :+ GP.Var("out"))
      ))
    }
    val edbDataBody = GP.Body(Seq(
      GP.HasType(GP.Var("out"), GP.TNode(data.name.name))
    ))
    val dataPat = GP.Pattern(None, data.name.name, Seq(GP.Param("out", typ)),
      constrBodies :+ edbDataBody
    ).addHint(DataHints.DataType)

    dataPat +: data.constrs.flatMap(transDataConstructor(_, vis, typ))
  }

  def lowerData(data: DataDef): GP.DataDef =
    GP.DataDef(transVis(data.vis), data.name.name, data.constrs.map {
      case DataConstructor(cname, paramTypes) => GP.DataConstructor(cname.name, paramTypes.map(lowerType))
    })

  val tyURI: meta.Type = typeOf[truechange.URI]
  val tDataURI: meta.Term = symbolOf(DataURI)

  private def transDataConstructor(constr: DataConstructor, vis: Option[GP.Visibility], typ: GP.Type): Seq[GP.Pattern] = {
    import scala.meta._

    val params = constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
      GP.Param(s"_$ix", transType(typ))
    }
    val outParam = GP.Param("out", typ)

    val constrScalaFun = Term.Function(
      params.map(p => Term.Param(Nil, Term.Name(p.name), Some(p.typ.asScala), None)).toList,
      q"""$tDataURI(${constr.name.name}, ..${params.map(p => Term.Name(p.name)).toList})"""
    )
    val outVar = GP.Var(outParam.name)
    val constrIDBBody = GP.Body(Seq(GP.Computed(outVar,
      GP.Evaluation(params.map(p => GP.Var(p.name) -> p.typ), typ, Scala(constrScalaFun))))).addHint(DataHints.IDBConstructor)

    val constrType = GP.TNode(constr.name.name)
    val constrEDBBody = GP.Body(
      GP.HasType(outVar, constrType) +:
        constr.paramTypes.zipWithIndex.map { case (typ, ix) =>
          GP.Path(outVar, constrType, GP.NamedLink(constrType, s"_$ix"), GP.Var(s"_$ix"), transRuntimeType(typ))
        }
    ).addHint(MagicSetHints.NoInputRelation)
    val constrPat = GP.Pattern(vis, constr.name.name, params :+ outParam, Seq(constrIDBBody, constrEDBBody))
      .addHint(DataHints.Constructor)

    val selectorCons = GP.Call(constr.name.name, (params :+ outParam).map(p => GP.Var(p.name)))
      .addHint(MagicSetHints.IgnoreCall)
      .addHint(MagicSetHints.FixedAdornment(params.map(_ => true) :+ false))
    val selectorPat = GP.Pattern(vis, constr.selectorName, outParam +: params, Seq(GP.Body(Seq(selectorCons))))
      .addHint(MagicSetHints.NoInputRelation)
      .addHint(DataHints.Selector)

    Seq(constrPat, selectorPat)
  }

  private def transVis(vis: Option[Visibility]): Option[GP.Visibility] =
    vis.map { case Private => GP.Private }

  @tailrec
  private def transType(typ: Type): GP.Type = typ match {
    case TAny => GP.TAny
    case TData(_) => GP.TScala(Scala(tyURI))
    case TScala(ty) => GP.TScala(ty)
    case TOption(ty) => transType(ty)
    case TSet(ty) => transType(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }

  @tailrec
  private def lowerType(typ: Type): GP.Type = typ match {
    case TAny => GP.TAny
    case TData(name) => GP.TData(name.name)
    case TScala(ty) => GP.TScala(ty)
    case TOption(ty) => lowerType(ty)
    case TSet(ty) => lowerType(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }

  private def transRuntimeType(typ: Type): GP.Type = typ match {
    case TAny => GP.TAny
    case TData(name) => GP.TNode(name.name)
    case TScala(Scala(meta.Type.Name(ty))) =>
      ty match {
        case "String" => GP.TLiteral.String
        case "Int" => GP.TLiteral.Int
        case "Boolean" => GP.TLiteral.Bool
        case "Long" => GP.TLiteral.Long
        case "Double" => GP.TLiteral.Double
        case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
      }
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
