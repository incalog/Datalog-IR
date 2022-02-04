package inca.frontend.functional

import inca.frontend.functional.core._


object CollectCalledFunctionNames extends Collect[Name] {
  override def transExp(exp: Expression): Seq[Name] = exp match {
    case Call(fun, args, transitive) => fun match {
      case Var(name) => Seq(name) ++ args.flatMap(super.transExp)
      case _ => super.transExp(exp)
    }
    case _ => super.transExp(exp)
  }
}

trait Collect[R] {

  def apply(module: Module): Seq[R] = {
    module.content.flatMap{
      case func: FunctionDef => transFun(func)
      case data: DataDef => transData(data)
    }
  }

  def transData(data: DataDef): Seq[R] = {
    data.annos.flatMap(transAnno) ++ data.constrs.flatMap(transConstr)
  }

  def transFun(func: FunctionDef): Seq[R] = {
    func.annos.flatMap(transAnno) ++ func.params.flatMap(transParam) ++ transExp(func.body)
  }

  def transAnno(anno: Annotation): Seq[R] = Seq()

  def transConstr(constr: DataConstructor): Seq[R] = Seq()

  def transParam(par: Param): Seq[R] = Seq()

  def transExp(exp: Expression): Seq[R] = {
    exp match {
      case Var(name) => Seq()
      case inca.frontend.functional.core.Let(names, anno, bound, body) =>
        transExp(bound) ++ transExp(body)
      case BaseApplyInfix(left, op, right) =>
        transExp(left)++transExp(right)
      case BaseLit(code) => Seq()
      case If(cnd, thn, els) =>
        transExp(cnd)++transExp(thn)++transExp(els)
      case Match(matchee: Expression, cases: Seq[(Pattern, Expression)]) =>
        transExp(matchee)++cases.flatMap(c => transExp(c._2))
      case Call(fun, args, transitive) => transExp(fun)++args.flatMap(transExp)
      case BaseApply(fun, args) =>
        args.flatMap(transExp)
      case Tuple(exps) =>
        exps.flatMap(transExp)
      case SetExp(es) =>
        es.flatMap(transExp)
      case SetComprehension(build, predicates) =>
        transExp(build)++predicates.flatMap(transExp)
      case SetMember(tup, set, neg) =>
        transExp(tup)++transExp(set)
      case Lambda(vs, body) => transExp(body)
      case SetFold(anno, init, op, set) =>
        transExp(init)++transExp(op)++transExp(set)
      case SomeExp(e) => transExp(e)
      case NoneExp() => Seq()
    }
  }
}
