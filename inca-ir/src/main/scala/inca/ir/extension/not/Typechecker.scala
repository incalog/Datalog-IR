package inca.ir.extension.not

import inca.ir.extension.not.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  // TODO Handle negation of call correctly
  override def typecheck(atom: Atom): Unit = atom match
    case Not(at) =>
      withBound(bound.flipped) {
        typecheck(at)
      }
    case _ => super.typecheck(atom)
