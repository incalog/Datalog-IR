package inca.ir.extension.demand

import inca.ir.Atom
import inca.ir.extension.disjunction.Disjunction
import inca.ir.typing.BaseIRTypechecker

trait Typechecker extends BaseIRTypechecker:
  override def typecheck(atom: Atom): Unit = atom match
    case Demand(ts) =>
      ts.foreach(typecheck(_, Bound.Assign))
    case _ =>
      super.typecheck(atom)