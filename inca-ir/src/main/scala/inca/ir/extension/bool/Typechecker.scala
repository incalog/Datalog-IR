package inca.ir.extension.bool

import inca.ir.extension.bool.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case AtomAsBool(at: Atom) =>
      checkAtom(at, Mode.Bound)
      TBoolean.bound
    case BoolAnd(t1, t2) =>
      checkTerm(t1, TBoolean, Mode.Bound)
      checkTerm(t2, TBoolean, Mode.Bound)
      TBoolean.bound
    case BoolOr(t1, t2) =>
      checkTerm(t1, TBoolean, Mode.Bound)
      checkTerm(t2, TBoolean, Mode.Bound)
      TBoolean.bound
    case BoolNot(t) =>
      checkTerm(t, TBoolean, Mode.Bound)
      TBoolean.bound
    case BoolFalse => TBoolean.bound
    case BoolTrue => TBoolean.bound
    case _ => super.inferTermExtend(term, mode)
