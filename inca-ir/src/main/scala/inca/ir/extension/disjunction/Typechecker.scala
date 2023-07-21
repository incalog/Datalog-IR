package inca.ir.extension.disjunction

import inca.ir.Atom
import inca.ir.extension.disjunction.Disjunction
import inca.ir.typing.BaseIRTypechecker

trait Typechecker extends BaseIRTypechecker:
  override def typecheck(atom: Atom): Unit = atom match
    case Disjunction(ass) =>
      ass.foreach(_.foreach(typecheck))
    case _ =>
      super.typecheck(atom)