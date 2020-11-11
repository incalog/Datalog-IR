package inca.frontend.desugar

import inca.frontend.core._
import inca.util.Gensym

/**
 * Desugaring adapter that makes no changes by default.
 * Acutal desugarings should extend this class and must set `changesMade` when making a change.
 */
class DesugarTrans {
  var changesMade: Boolean = false

  def changed[T](t: T): T = {
    changesMade = true
    t
  }

  def desugarModule(module: Module)(implicit gensym: Gensym): Module = gensym.scoped {
    val Module(name, imports, content) = module
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedDefNames.map(_.name))
    Module(name, imports.flatMap(desugarImport), content.flatMap(desugarModuleContent))
  }

  def desugarImport(imp: Import)(implicit gensym: Gensym): Seq[Import] =
    Seq(Import(imp.name))

  def desugarModuleContent(content: ModuleContent)(implicit gensym: Gensym): Seq[ModuleContent] = content match {
    case fun: PatternFunction => desugarFun(fun)
    case ValDef(vis, name, typ, exp) => Seq(ValDef(vis, name, typ, desugarExp(exp)))
    case con => Seq(con)
  }

  def desugarFun(fun: PatternFunction)(implicit gensym: Gensym): Seq[PatternFunction] = gensym.scoped {
    gensym.register(fun.boundNames.map(_.name))
    val newfun = PatternFunction(fun.vis, fun.name, fun.params, fun.outParams, fun.bodies.flatMap(desugarBody))
    Seq(newfun)
  }

  def desugarBody(body: Body)(implicit gensym: Gensym): Seq[Body] =
    Seq(Body(body.stmts.flatMap(desugarStm)))

  def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
    case Assert(cond) => Seq(Assert(desugarExp(cond)))
    case Values(_, _) => Seq(stm)
    case Assign(names, exp) => Seq(Assign(names, desugarExp(exp)))
    case Yield(exp) => Seq(Yield(desugarExp(exp)))
    case _ =>
      Seq(stm)
  }

  def desugarExp(exp: Expression)(implicit gensym: Gensym): Expression = (exp match {
    case Eq(lhs, rhs) => Eq(desugarExp(lhs), desugarExp(rhs))
    case Neq(lhs, rhs) => Neq(desugarExp(lhs), desugarExp(rhs))
    case InstanceOf(exp, typ) => InstanceOf(desugarExp(exp), typ)
    case NotInstanceOf(exp, typ) => NotInstanceOf(desugarExp(exp), typ)
    case Cast(e, ty) => Cast(desugarExp(e), ty)
    case Def(exp) => Def(desugarExp(exp))
    case Undef(exp) => Undef(desugarExp(exp))
    case Var(name) => Var(name)
    case Constant(lit) => Constant(lit)
    case PathAccess(receiver, link) => PathAccess(desugarExp(receiver), link)
    case Call(name, args, trans) => Call(name, args.map(desugarExp), trans)
    case Count(call@Call(name, args, trans)) => Count(Call(name, args.map(desugarExp), trans).mtyped(call.typ))
    case Tuple(exps) => Tuple(exps.map(desugarExp))
    case Aggregate(agg, bodies) => Aggregate(desugarExp(agg), bodies.flatMap(desugarBody))
    case eval@Eval(code) =>
      val desugaredEval = Eval(code)
      eval.params match {
        case Some(params) =>
          desugaredEval.params = Some(params.map(p => EvalParam(p.name)))
        case None => // nothing
      }
      desugaredEval
    case _ => exp
  }).mtyped(exp.typ)
}
