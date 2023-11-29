package inca.ir.extension.not

import inca.ir.extension.not.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Not(at) =>
      checkAtom(at, mode.inverted)
    case WeakNot(at) =>
      checkAtom(at, mode.weakInverted)
    case _ => super.checkAtom(atom, mode)
