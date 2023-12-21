package inca.ir.extension.disjunction

import inca.ir.Atom
import inca.ir.extension.disjunction.Disjunction
import inca.ir.typing.{BaseIRTypechecker, Mode}

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Disjunction(alts) =>
      checkAlternatives(alts)(alt => alt.body.atoms.foreach(checkAtom(_, mode)))
    case _ => super.checkAtom(atom, mode)