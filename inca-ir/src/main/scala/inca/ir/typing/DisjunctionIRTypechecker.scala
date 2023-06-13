package inca.ir.typing
import inca.ir.Atom
import inca.ir.extensions.Disjunction

trait DisjunctionIRTypechecker extends BaseIRTypechecker:
  override def typecheck(atom: Atom): Unit = atom match
    case Disjunction(as1, as2) =>
      as1.foreach(typecheck)
      as2.foreach(typecheck)
    case _ =>
      super.typecheck(atom)