package inca.frontend.functional.syntax

class Visitor:
  def visitModule(m: Module): Module =
    val content = m.content.map {
      case d: DataDef => visitDataDef(d)
      case f: FunctionDef => visitFunDef(f)
    }
    m.copy(content = content)

  def visitDataDef(data: DataDef): DataDef =
    data

  def visitFunDef(fun: FunctionDef): FunctionDef =
    fun.copy(body = visitExp(fun.body))

  def visitExp(e: Expression): Expression = e match
    case BinOp(e1, op, e2) => BinOp(visitExp(e1), op, e2)
    case Call(f, ty, args) => Call(visitExp(f), ty, args.map(visitExp))
    case If(c, t, e) => If(visitExp(c), visitExp(t), visitExp(e))
    case Lambda(vs, body) => Lambda(vs, visitExp(body))
    case Let(names, anno, bound, body) => Let(names, anno, visitExp(bound), visitExp(body))
    case Match(matchee, cases) => Match(visitExp(matchee), cases.map { case (p,e) => (p, visitExp(e)) })
    case SetComprehension(build, predicates) => SetComprehension(visitExp(build), predicates.map(visitExp))
    case SetExp(es) => SetExp(es.map(visitExp))
    case SetFold(anno, init, op, set) => SetFold(anno, visitExp(init), visitExp(op), visitExp(set))
    case SetMember(tup, set, neg) => SetMember(visitExp(tup), visitExp(set), neg)
    case Tuple(es) => Tuple(es.map(visitExp))
    case UnOp(op, e) => UnOp(op, visitExp(e))
    case _ => e
