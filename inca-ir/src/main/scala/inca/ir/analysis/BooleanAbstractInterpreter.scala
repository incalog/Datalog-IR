package inca.ir.analysis

import inca.ir.extension.bool.{AtomAsBool, BoolAnd, BoolFalse, BoolNot, BoolOr, BoolTrue}
import inca.ir.Term

trait BooleanAbstractInterpreter[B] extends BaseAbstractInterpreter[B, B]:

  override def evalTermExtend(term: Term): TermResult = term match
    case BoolFalse => TermResult(boolOps.boolLit(false), trueBool)
    case BoolTrue => TermResult(boolOps.boolLit(true), trueBool)
    case BoolAnd(t1, t2) =>
      val TermResult(v1, p1) = evalTerm(t1)
      val TermResult(v2, p2) = evalTerm(t2)
      TermResult(boolOps.and(v1, v2), boolOps.and(p1, p2))
    case BoolOr(t1, t2) =>
      val TermResult(v1, p1) = evalTerm(t1)
      val TermResult(v2, p2) = evalTerm(t2)
      TermResult(boolOps.or(v1, v2), boolOps.and(p1, p2))
    case BoolNot(t) =>
      val TermResult(v, p) = evalTerm(t)
      TermResult(boolOps.not(v), p)
    case AtomAsBool(atom) =>
      val AtomResult(v, p) = evalAtom(atom)
      TermResult(v, p)
    case _ => super.evalTermExtend(term)
