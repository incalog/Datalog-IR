package inca.ir.analysis

import inca.ir.extension.bool.{AtomAsBool, BoolAnd, BoolFalse, BoolNot, BoolOr, BoolTrue}
import inca.ir.Term

trait BooleanAbstractInterpreter[V, B](v2b: V => B, b2v: B => V) extends BaseAbstractInterpreter[V, B]:

  override def evalTermExtend(term: Term): TermResult = term match
    case BoolFalse => TermResult(b2v(boolOps.boolLit(false)), trueBool)
    case BoolTrue => TermResult(b2v(boolOps.boolLit(true)), trueBool)
    case BoolAnd(t1, t2) =>
      val TermResult(v1, p1) = evalTerm(t1)
      val TermResult(v2, p2) = evalTerm(t2)
      TermResult(b2v(boolOps.and(v2b(v1), v2b(v2))), boolOps.and(p1, p2))
    case BoolOr(t1, t2) =>
      val TermResult(v1, p1) = evalTerm(t1)
      val TermResult(v2, p2) = evalTerm(t2)
      TermResult(b2v(boolOps.or(v2b(v1), v2b(v2))), boolOps.and(p1, p2))
    case BoolNot(t) =>
      val TermResult(v, p) = evalTerm(t)
      TermResult(b2v(boolOps.not(v2b(v))), p)
    case AtomAsBool(atom) =>
      val AtomResult(v, p) = evalAtom(atom)
      TermResult(b2v(v), p)
    case _ => super.evalTermExtend(term)
