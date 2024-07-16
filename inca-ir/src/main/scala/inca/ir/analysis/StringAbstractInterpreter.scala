package inca.ir.analysis

import inca.ir.extension.string.*
import inca.ir.{Atom, Term}


trait StringAbstractInterpreter[V, B] extends BaseAbstractInterpreter[V, B]:

  val stringOps: StringOps[V]

  override def evalTermExtend(term: Term): TermResult = term match
    case StringLit(s) =>
      TermResult(stringOps.stringLit(s), trueBool)
    case StringConcat(t1, t2) =>
      val TermResult(v1, p1) = evalTerm(t1)
      val TermResult(v2, p2) = evalTerm(t2)
      TermResult(stringOps.concat(v1, v2), boolOps.and(p1, p2))
    case ToString(t) =>
      val TermResult(v, p) = evalTerm(t)
      TermResult(stringOps.toString(v), p)
    case _ => super.evalTermExtend(term)