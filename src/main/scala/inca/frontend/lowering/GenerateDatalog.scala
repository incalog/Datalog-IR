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
      val condRes = transExp(cnd.ensureCore)
      val thnRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condRes;
             (thnTerm, thnCons) <- transExp(thn.ensureCore))
          yield (thnTerm, cndCons ++ Seq(GP.Eq(cndTerm, GP.True)) ++ thnCons)
      val elsRes: ExpRes =
        for ((Seq(cndTerm), cndCons) <- condRes;
             (elsTerm, elsCons) <- transExp(els.ensureCore))
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

    case eval@Eval(code) =>
      import scala.meta._
      val evalOut = GP.Var(gensym.fresh("eval"))
      val params = eval.params.getOrElse(Seq())
      val paramsTyped = params.map { param =>
        val paramTyp = param.typ.getOrElse(throw new IllegalStateException(s"Cannot compile eval with untyped param $param"))
        param"${Term.Name(param.name.name)}: ${paramTyp.asScala}"
      }.toList
      val funCode = q"(..$paramsTyped) => ${code.tree}"
      val args = params.map { param => (GP.Var(param.name.name), transType(param.typ.get)) }
      val resType = eval.typ.getOrElse(throw new IllegalStateException("cannot compile untyped Eval"))
      val evalConstraint = GP.Computed(evalOut, GP.Evaluation(args, transType(resType), Scala(funCode)))
      Seq((Seq(evalOut), Seq(evalConstraint)))
  }

  private def transType(typ: Type): GP.Type = typ match {
    case TAny => GP.TAny
    case TScala(ty) => GP.TScala(ty)
    case _ => throw new IllegalArgumentException(s"Cannot translate $typ to Datalog")
  }
}
