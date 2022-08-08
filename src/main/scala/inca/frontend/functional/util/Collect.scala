package inca.frontend.functional.util

import inca.frontend.functional.core._

trait Collect[R] {

  def apply(module: Module): Seq[R] = module.content.flatMap {
    case fun: FunctionDef => collectFunctionDef(fun)
    case data: DataDef => collectDataDef(data)
  }

  def collectDataDef(data: DataDef): Seq[R] =
    data.annos.flatMap(collectAnnotation) ++ data.constrs.flatMap(collectDataConstructor)

  def collectFunctionDef(fun: FunctionDef): Seq[R] =
    fun.annos.flatMap(collectAnnotation) ++ fun.params.flatMap(collectParam) ++ collectExpression(fun.body)

  def collectAnnotation(anno: Annotation): Seq[R] = Seq()

  def collectType(t: Type): Seq[R] = t match {
    case TAny => Seq()
    case TNothing => Seq()
    case TFun(from, to) => from.flatMap(collectType) ++ collectType(to)
    case TTuple(ts) => ts.flatMap(collectType)
    case TName(_) => Seq()
    case TConstr(_, tys) => tys.flatMap(collectType)
    case TScala(_) => Seq()
    case TOption(ty) => collectType(ty)
    case TSet(ty) => collectType(ty)
  }

  def collectDataConstructor(constr: DataConstructor): Seq[R] = constr.paramTypes.flatMap(collectType)

  def collectParam(par: Param): Seq[R] = collectType(par.typ)

  def collectExpression(exp: Expression): Seq[R] = exp match {
    case Var(name) => Seq()
    case Let(names, anno, bound, body) =>
      collectExpression(bound) ++ collectExpression(body) ++ collectType(anno.getOrElse(TAny))
    case BaseApplyInfix(left, op, right) =>
      collectExpression(left) ++ collectExpression(right)
    case BaseLit(code) => Seq()
    case BaseApplyUnary(op, e) => collectExpression(e)
    case BaseApplyMethod(recv, meth, args) => collectExpression(recv) ++ args.getOrElse(Seq()).flatMap(collectExpression)
    case BaseApply(fun, args) =>
      args.flatMap(collectExpression)
    case If(cnd, thn, els) =>
      collectExpression(cnd) ++ collectExpression(thn) ++ collectExpression(els)
    case Match(matchee: Expression, cases: Seq[(Pattern, Expression)]) =>
      collectExpression(matchee) ++ cases.flatMap(c => collectExpression(c._2))
    case Call(fun, tyArgs, args, transitive) => collectExpression(fun) ++ tyArgs.flatMap(collectType) ++ args.flatMap(collectExpression)
    case Tuple(exps) =>
      exps.flatMap(collectExpression)
    case SetExp(es) =>
      es.flatMap(collectExpression)
    case SetComprehension(build, predicates) =>
      collectExpression(build) ++ predicates.flatMap(collectExpression)
    case SetMember(tup, set, neg) =>
      collectExpression(tup) ++ collectExpression(set)
    case Lambda(vs, body) => vs.flatMap(v => collectType(v._2)) ++ collectExpression(body)
    case SetFold(anno, init, op, set) =>
      collectType(anno.getOrElse(TAny)) ++ collectExpression(init) ++ collectExpression(op) ++ collectExpression(set)
    case SomeExp(e) => collectExpression(e)
    case NoneExp() => Seq()
    case TypeCast(e, ty) => collectExpression(e)
  }
}
