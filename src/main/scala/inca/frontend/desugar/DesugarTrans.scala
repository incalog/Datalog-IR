package inca.frontend.desugar

import inca.frontend.core.Core._
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
    gensym.register(module.usedModuleNames.map(_.name))
    gensym.register(module.usedFunNames.map(_.name))
    Module(module.name, module.imports.flatMap(desugarImport), module.funs.flatMap(desugarFun), module.stats)
  }

  def desugarImport(imp: Name)(implicit gensym: Gensym): Seq[Name] =
    Seq(imp)

  def desugarFun(fun: PatternFunction)(implicit gensym: Gensym): Seq[PatternFunction] = gensym.scoped {
    gensym.register(fun.boundNames.map(_.name))
    Seq(PatternFunction(fun.vis, fun.name, fun.params, fun.outParams, fun.bodies.flatMap(desugarBody)))
  }

  def desugarBody(body: Body)(implicit gensym: Gensym): Seq[Body] =
    Seq(Body(body.stmts.flatMap(desugarStm)))

  def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
    case Assert(cond) => Seq(Assert(desugarExp(cond)))
    case Assign(names, exp) => Seq(Assign(names, desugarExp(exp)))
    case Yield(exp) => Seq(Yield(desugarExp(exp)))
    case _ => Seq(stm)
  }

  def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = (exp match {
    case Eq(lhs, rhs) => Eq(desugarExp(lhs), desugarExp(rhs))
    case Neq(lhs, rhs) => Neq(desugarExp(lhs), desugarExp(rhs))
    case InstanceOf(exp, typ) => InstanceOf(desugarExp(exp), typ)
    case NotInstanceOf(exp, typ) => NotInstanceOf(desugarExp(exp), typ)
    case Def(exp) => Def(desugarExp(exp))
    case Undef(exp) => Undef(desugarExp(exp))
    case Var(name) => Var(name)
    case Constant(lit) => Constant(lit)
    case PathAccess(receiver, link) => PathAccess(desugarExp(receiver), link)
    case Call(name, args, trans) => Call(name, args.map(desugarExp), trans)
    case Count(call@Call(name, args, trans)) => Count(Call(name, args.map(desugarExp), trans).mtyped(call.typ))
    case Tuple(exps) => Tuple(exps.map(desugarExp))
    case Aggregate(init, join, unjoin, call@Call(name, args, trans)) => Aggregate(init, join, unjoin, Call(name, args.map(desugarExp), trans).mtyped(call.typ))
    case _ => exp
  }).mtyped(exp.typ)
}
