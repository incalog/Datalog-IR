package inca.ir.extension.not

import inca.ir.extension.not.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def typecheck(atom: Atom): Unit = atom match
    case Not(at) => typecheck(at)
    case _ => super.typecheck(atom)
