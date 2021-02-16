package inca.frontend.lowering

import inca.backend.ir.GP
import inca.frontend.core._
import inca.util.Meta.Scala
import inca.util.{Gensym, TupleOps}

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

    val ScalaModuleContents = ListBuffer[meta.Import]()
    val blockDefs = ListBuffer[meta.Stat]()
    contents.foreach {
      case fun: FunctionDef => generatedPatterns += transFun(fun)
      case _: ValDef => ??? // will be inlined
    }

    val scalaContent = ScalaModuleContents.toList ++ blockDefs.toList
    GP.Module(name.name, imports.map(_.name.name), generatedPatterns.toList, scalaContent.map(Scala.apply))
  }

  private def transFun(fun: FunctionDef): GP.Pattern = gensym.scoped {
    gensym.register(fun.vars.keys.map(_.name))

    val vis = fun.vis.map { case Private => GP.Private }
    val params = fun.params.flatMap(p => flattenParam(p.name.name, p.typ, genFresh = false))
    val outParams = flattenParam("out", fun.outType, genFresh = true)

    val bodies = for ((terms, cons) <- transExp(fun.body.ensureCore))
      yield GP.Body(cons ++ outParams.zip(terms).map(pt => GP.Eq(GP.Var(pt._1.name), pt._2)))

    GP.Pattern(vis, fun.name.name, params ++ outParams, bodies)
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
        case Some(target) => throw new IllegalArgumentException(s"Unknown call target $target")
        case None => throw new IllegalArgumentException(s"Unresolved call $call")
      }
      val argRes = args.map(e => transExp(e.ensureCore))
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

  }

  private def transType(typ: Type): GP.Type = typ match {
    case TAny => GP.TAny
    case TScala(ty) => GP.TScala(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
