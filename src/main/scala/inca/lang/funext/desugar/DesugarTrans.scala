package inca.lang.funext.desugar

import inca.lang.fun.Fun._
import inca.util.Gensym

/**
 * Desugaring adapter that makes no changed by default.
 * Acutal desugarings should extend this class and must set `changesMade` when making a change.
 */
class DesugarTrans {
  var changesMade: Boolean = false

  def changed[T](t: T): T = {
    changesMade = true
    t
  }

  def desugarModule(module: Module)(implicit gensym: Gensym): Module = gensym.scoped {
    module.usedModuleNames.foreach(gensym.register)
    module.usedFunNames.foreach(gensym.register)
    Module(module.name, module.imports.flatMap(desugarImport), module.funs.flatMap(desugarFun))
  }

  def desugarImport(imp: Name)(implicit gensym: Gensym): Seq[Name] =
    Seq(imp)

  def desugarFun(fun: PatternFunction)(implicit gensym: Gensym): Seq[PatternFunction] = gensym.scoped {
    gensym.register(fun.boundNames)
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
    case Call(name, args, trans, count) => Call(name, args.map(desugarExp), trans, count)
    case Tuple(exps) => Tuple(exps.map(desugarExp))
    case _ => exp
  }).mtyped(exp.typ)
}
