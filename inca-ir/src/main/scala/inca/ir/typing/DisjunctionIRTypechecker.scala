package inca.ir.typing
import inca.ir.Atom
import inca.ir.extension.disjunction.Disjunction

trait DisjunctionIRTypechecker extends BaseIRTypechecker:
  override def typecheck(atom: Atom): Unit = atom match
    case Disjunction(ass) =>
      ass.foreach(_.foreach(typecheck))
    case _ =>
      super.typecheck(atom)