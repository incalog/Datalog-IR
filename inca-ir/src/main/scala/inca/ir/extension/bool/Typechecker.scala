package inca.ir.extension.bool

import inca.ir.extension.bool.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case BoolAtom(t) => checkTerm(t, TBoolean, mode)
    case _ => super.checkAtom(atom, mode)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case AtomAsBool(at: Atom) =>
      checkAtom(at, Mode.Bound)
      TBoolean.closed
    case BoolAnd(t1, t2) =>
      checkTerm(t1, TBoolean, Mode.Bound)
      checkTerm(t2, TBoolean, Mode.Bound)
      TBoolean.closed
    case BoolOr(t1, t2) =>
      checkTerm(t1, TBoolean, Mode.Bound)
      checkTerm(t2, TBoolean, Mode.Bound)
      TBoolean.closed
    case BoolNot(t) =>
      checkTerm(t, TBoolean, Mode.Bound)
      TBoolean.closed
    case BoolFalse => TBoolean.closed
    case BoolTrue => TBoolean.closed
    case _ => super.inferTermExtend(term, mode)
