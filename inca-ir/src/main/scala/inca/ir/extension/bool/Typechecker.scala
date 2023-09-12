package inca.ir.extension.bool

import inca.ir.extension.bool.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case BoolAtom(t) => checkTerm(t, TBoolean, mode)
    case _ => super.checkAtom(atom, mode)

  override def inferTermExtend(term: Term, mode: Mode): Type = term match
    case AtomAsBool(at: Atom) =>
      checkAtom(at, mode)
      TBoolean
    case BoolAnd(t1, t2) =>
      checkTerm(t1, TBoolean, Mode.Closed)
      checkTerm(t2, TBoolean, Mode.Closed)
      TBoolean
    case BoolOr(t1, t2) =>
      checkTerm(t1, TBoolean, Mode.Closed)
      checkTerm(t2, TBoolean, Mode.Closed)
      TBoolean
    case BoolNot(t) =>
      checkTerm(t, TBoolean, Mode.Closed)
      TBoolean
    case BoolFalse => TBoolean
    case BoolTrue => TBoolean
    case _ => super.inferTermExtend(term, mode)
