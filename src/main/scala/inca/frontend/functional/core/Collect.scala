package inca.frontend.functional.core

trait Collect[R] {
  def transModule(module: Module): Seq[R] = {
    module.content.flatMap {
      case fun: FunctionDef => transFunDef(fun)
      case data: DataDef => transDataDef(data)
    }
  }

  def transFunDef(fun: FunctionDef): Seq[R] = {
    fun.vis.toSeq.flatMap(transVisibility) ++
      fun.params.flatMap(transParam) ++
      transExpression(fun.body)
  }

  def transVisibility(vis: Visibility): Seq[R] = Seq()
  def transParam(param: Param): Seq[R] = transType(param.typ)
  def transDataDef(data: DataDef): Seq[R] = Seq()
  def transType(ty: Type): Seq[R] = Seq()

  def transExpression(exp: Expression): Seq[R] = exp match {
    case Let(_, anno, bound, body) =>
      anno.toSeq.flatMap(transType) ++ transExpression(bound) ++ transExpression(body)
    case Var(_) => Seq()
    case If(cnd, thn, els) =>
      transExpression(cnd) ++ transExpression(thn) ++ transExpression(els)
    case TypeCast(exp, ty) =>
      transExpression(exp) ++ transType(ty)
    case Call(fun, args, _) =>
      transExpression(fun) ++ args.flatMap(transExpression)
    case Lambda(vs, body) =>
      vs.flatMap(x => transType(x._2)) ++ transExpression(body)
    case Tuple(exps) =>
      exps.flatMap(transExpression)
    case Match(matchee, cases) =>
      transExpression(matchee) ++ cases.flatMap(transCase)
    case BaseLit(_) => Seq()
    case BaseApply(_, args) => args.flatMap(transExpression)
    case BaseApplyInfix(left, _, right) =>
      transExpression(left) ++ transExpression(right)
    case NoneExp() => Seq()
    case SomeExp(e) => transExpression(e)
    case SetExp(es) => es.flatMap(transExpression)
    case SetComprehension(build, predicates) =>
      transExpression(build) ++ predicates.flatMap(transExpression)
    case SetMember(tup, set, _) =>
      transExpression(tup) ++ transExpression(set)
    case SetFold(anno, init, op, set) => anno.toSeq.flatMap(transType) ++ transExpression(init) ++ transExpression(op) ++ transExpression(set)
    case BaseApplyUnary(_, e) => transExpression(e)
    case BaseApplyMethod(recv, _, args) => transExpression(recv) ++ args.getOrElse(Seq()).flatMap(transExpression)
  }

  def transCase(c: (Pattern, Expression)): Seq[R] =
    transPattern(c._1) ++ transExpression(c._2)

  def transPattern(p: Pattern): Seq[R] = Seq()
}
