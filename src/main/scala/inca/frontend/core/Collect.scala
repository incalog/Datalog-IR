package inca.frontend.core

import inca.frontend.core.tree._

object CollectUndefPaths extends Collect[PathAccess] {
  override def transExp(exp: CoreExpression): Seq[PathAccess] = exp match {
    case Undef(acc: PathAccess) => acc +: super.transExp(acc)
    case _ => super.transExp(exp)
  }
}

object CollectNotInstanceOfTypes extends Collect[Type] {
  override def transExp(exp: CoreExpression): Seq[Type] = exp match {
    case NotInstanceOf(e, ty) => ty +: super.transExp(e.ensureCore)
    case _ => super.transExp(exp)
  }
}

trait Collect[R] {

  def transModule(module: Module): Seq[R] = module.content.flatMap {
    case fun: PatternFunction => transFun(fun)
    case _: ScalaModuleContent => Seq()
  }

  def transFun(fun: PatternFunction): Seq[R] = {
    val paramsRes = fun.params.flatMap(transParam)
    paramsRes ++ fun.bodies.flatMap(transBody)
  }

  def transParam(param: Param): Seq[R] = Seq()

  def transBody(body: Body): Seq[R] = body.stmts.flatMap(s => transStatement(s.ensureCore))

  def transStatement(stm: CoreStatement): Seq[R] = stm match {
    case Values(name, typ) => transBinding(name)
    case Assign(names, exp) => names.flatMap(transBinding) ++ transExp(exp.ensureCore)
    case Assert(cond) => transExp(cond.ensureCore)
    case Yield(exp) => transExp(exp.ensureCore)
    case FailStatement => Seq()
  }

  def transBinding(name: Name): Seq[R] = Seq()
  def transReference(name: Name): Seq[R] = Seq()

  def transExp(exp: CoreExpression): Seq[R] = exp match {
    case Eq(lhs, rhs) => transExp(lhs.ensureCore) ++ transExp(rhs.ensureCore)
    case Neq(lhs, rhs) => transExp(lhs.ensureCore) ++ transExp(rhs.ensureCore)
    case InstanceOf(exp, ty) => transExp(exp.ensureCore)
    case NotInstanceOf(exp, ty) => transExp(exp.ensureCore)
    case Def(exp) => transExp(exp.ensureCore)
    case Undef(exp) => transExp(exp.ensureCore)
    case Var(name) => transReference(name)
    case Constant(lit) => transLit(lit)
    case PathAccess(receiver, link) => transExp(receiver.ensureCore)
    case Call(name, args, transitive) => args.flatMap(a => transExp(a.ensureCore))
    case Count(Call(name, args, transitive)) => args.flatMap(a => transExp(a.ensureCore))
    case Tuple(exps) => exps.flatMap(e => transExp(e.ensureCore))
    case eval: Eval => eval.params.get.flatMap(p => transReference(p.name))
    case Aggregate(agg, bodies) => transExp(agg.ensureCore) ++ bodies.flatMap(transBody)
  }

  def transLit(lit: Literal): Seq[R] = lit match {
    case IntLiteral(v) => Seq()
    case LongLiteral(v) => Seq()
    case DoubleLiteral(v) => Seq()
    case StringLiteral(v) => Seq()
    case BooleanLiteral(v) => Seq()
    case UnitLiteral => Seq()
  }
}
