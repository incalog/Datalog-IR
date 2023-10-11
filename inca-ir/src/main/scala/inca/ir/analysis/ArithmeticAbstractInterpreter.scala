package inca.ir.analysis

import inca.ir.{Atom, Term}
import inca.ir.extension.arithmetic.*


trait ArithmeticAbstractInterpreter[V, B] extends BaseAbstractInterpreter[V, B]:

  val intOps: IntegerOps[Int, V]
  val doubleOps: FloatOps[Double, V]
  val intOrderingOps: OrderingOps[V, B]
  val doubleOrderingOps: OrderingOps[V, B]

  override def evalAtomExtend(at: Atom): AtomResult = at match
    case BinCompare(t1, t2, op) =>
      val TermResult(v1, p1) = evalTerm(t1)
      val TermResult(v2, p2) = evalTerm(t2)
      (op, t1.typ.get.ty) match
        case ("<", TInt) => AtomResult(intOrderingOps.lt(v1, v2), boolOps.and(p1, p2))
        case ("<", TDouble) => AtomResult(doubleOrderingOps.lt(v1, v2), boolOps.and(p1, p2))
        case ("<=", TInt) => AtomResult(intOrderingOps.le(v1, v2), boolOps.and(p1, p2))
        case ("<=", TDouble) => AtomResult(doubleOrderingOps.le(v1, v2), boolOps.and(p1, p2))
        case (">", TInt) => AtomResult(intOrderingOps.gt(v1, v2), boolOps.and(p1, p2))
        case (">", TDouble) => AtomResult(doubleOrderingOps.gt(v1, v2), boolOps.and(p1, p2))
        case (">=", TInt) => AtomResult(intOrderingOps.ge(v1, v2), boolOps.and(p1, p2))
        case (">=", TDouble) => AtomResult(doubleOrderingOps.ge(v1, v2), boolOps.and(p1, p2))
    case _ => super.evalAtomExtend(at)

  override def evalTermExtend(term: Term): TermResult = term match
    case IntNum(i) => TermResult(intOps.integerLit(i), trueBool)
    case DoubleNum(d) => TermResult(doubleOps.floatingLit(d), trueBool)
    case BinOp(t1, t2, op) =>
      val TermResult(v1, p1) = evalTerm(t1)
      val TermResult(v2, p2) = evalTerm(t2)
      (op, term.typ.get.ty) match
        case ("+", TInt) => TermResult(intOps.add(v1, v2), boolOps.and(p1, p2))
        case ("+", TDouble) => TermResult(doubleOps.add(v1, v2), boolOps.and(p1, p2))
        case ("-", TInt) => TermResult(intOps.sub(v1, v2), boolOps.and(p1, p2))
        case ("-", TDouble) => TermResult(doubleOps.sub(v1, v2), boolOps.and(p1, p2))
        case ("*", TInt) => TermResult(intOps.mul(v1, v2), boolOps.and(p1, p2))
        case ("*", TDouble) => TermResult(doubleOps.mul(v1, v2), boolOps.and(p1, p2))
        case ("/", TInt) => TermResult(intOps.div(v1, v2), boolOps.and(p1, p2))
        case ("/", TDouble) => TermResult(doubleOps.div(v1, v2), boolOps.and(p1, p2))
        case ("%", TInt) => TermResult(intOps.remainder(v1, v2), boolOps.and(p1, p2))
        case ("min", TInt) => TermResult(intOps.min(v1, v2), boolOps.and(p1, p2))
        case ("min", TDouble) => TermResult(doubleOps.min(v1, v2), boolOps.and(p1, p2))
        case ("max", TInt) => TermResult(intOps.max(v1, v2), boolOps.and(p1, p2))
        case ("max", TDouble) => TermResult(doubleOps.max(v1, v2), boolOps.and(p1, p2))
    case UnOp(t, op) =>
      val TermResult(v, p) = evalTerm(t)
      (op, term.typ.get.ty) match
        case ("abs", TInt) => TermResult(intOps.absolute(v), p)
        case ("abs", TDouble) => TermResult(doubleOps.absolute(v), p)
    case _ => super.evalTermExtend(term)