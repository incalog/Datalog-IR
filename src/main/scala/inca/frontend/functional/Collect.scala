package inca.frontend.functional

import inca.frontend.functional.core._

trait Collect[R] {

  def apply(module: Module): Seq[R] = module.content.flatMap {
    case func: FunctionDef => transFun(func)
    case data: DataDef => transData(data)
  }

  def transData(data: DataDef): Seq[R] =
    data.annos.flatMap(transAnno) ++ data.constrs.flatMap(transConstr)

  def transFun(func: FunctionDef): Seq[R] =
    func.annos.flatMap(transAnno) ++ func.params.flatMap(transParam) ++ transExp(func.body)

  def transAnno(anno: Annotation): Seq[R] = Seq()

  def transType(t: Type): Seq[R] = {
    t match {
      case TAny => Seq()
      case TNothing => Seq()
      case TFun(from, to) => from.flatMap(transType) ++ transType(to)
      case TTuple(ts) => ts.flatMap(transType)
      case TData(name) => Seq()
      case TScala(ty) => Seq()
      case TOption(ty) => transType(ty)
      case TSet(ty) => transType(ty)
    }
  }

  def transConstr(constr: DataConstructor): Seq[R] = {
    constr.paramTypes.flatMap(transType)
  }

  def transParam(par: Param): Seq[R] = {
    transType(par.typ)
  }

  def transExp(exp: Expression): Seq[R] = {
    exp match {
      case Var(name) => Seq()
      case inca.frontend.functional.core.Let(names, anno, bound, body) =>
        transExp(bound) ++ transExp(body) ++ transType(anno.getOrElse(TAny))
      case BaseApplyInfix(left, op, right) =>
        transExp(left) ++ transExp(right)
      case BaseLit(code) => Seq()
      case If(cnd, thn, els) =>
        transExp(cnd) ++ transExp(thn) ++ transExp(els)
      case Match(matchee: Expression, cases: Seq[(Pattern, Expression)]) =>
        transExp(matchee) ++ cases.flatMap(c => transExp(c._2))
      case Call(fun, args, transitive) => transExp(fun) ++ args.flatMap(transExp)
      case BaseApply(fun, args) =>
        args.flatMap(transExp)
      case Tuple(exps) =>
        exps.flatMap(transExp)
      case SetExp(es) =>
        es.flatMap(transExp)
      case SetComprehension(build, predicates) =>
        transExp(build) ++ predicates.flatMap(transExp)
      case SetMember(tup, set, neg) =>
        transExp(tup) ++ transExp(set)
      case Lambda(vs, body) => vs.flatMap(v => transType(v._2)) ++ transExp(body)
      case SetFold(anno, init, op, set) =>
        transType(anno.getOrElse(TAny)) ++ transExp(init) ++ transExp(op) ++ transExp(set)
      case SomeExp(e) => transExp(e)
      case NoneExp() => Seq()
    }
  }
}
