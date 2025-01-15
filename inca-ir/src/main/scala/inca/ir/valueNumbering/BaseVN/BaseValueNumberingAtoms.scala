package inca.ir.valueNumbering.BaseVN

import inca.ir
import inca.ir.*
import inca.ir.valueNumbering.VNTables.*


trait BaseValueNumberingAtoms extends BaseValueNumberingTerms {

  private var VNs_Atoms = ValueIds[Atom]()

  override private[BaseVN] def setTables(body: Body): Unit = {
    phase match {
      case Phase.initial =>
        // reset congrClasses (otherwise not known when variables are unbound)
        VNs_Atoms = ValueIds[Atom]()
      case Phase.repetition =>
        VNs_Atoms = ValueIds[Atom]() // no need to propagate old analysis results -> remove duplicates again
    }
    super.setTables(body)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = {
    val newAtom = super.visitAtom(atom)
    valueNumberAtoms(newAtom)
  }


  protected def normalizeAtom(atom: Atom): Seq[Atom] = atom match {
    case Eq(lhs, rhs, false) if lhs == rhs => Seq()
    case Eq(lhs, rhs, true) if isConst(lhs) && isConst(rhs) && lhs != rhs => Seq()
    case Eq(Var(lhs), Var(rhs), true) if lhs == rhs => validBody = false; Seq(atom)
    case Eq(lhs, rhs, true) if isConst(lhs) && isConst(rhs) && lhs == rhs => validBody = false; Seq(atom)
    case Eq(lhs, rhs@Var(_), false) if rhs.mode.isBinding => Seq(Eq(rhs, lhs, false))
    case Eq(lhs, rhs, bool) if getIdOf(lhs) > getIdOf(rhs) && !lhs.mode.isBinding => Seq(Eq(rhs, lhs, bool))
    case _ => Seq(atom)
  }


  private def valueNumberAtoms(atomSeq: Seq[Atom]): Seq[Atom] = {
    if (atomSeq.isEmpty) return atomSeq
    val atom = normalizeAtom(atomSeq.head) match {
      case h :: _ => h
      case _ => return Seq()
    }

    if (VNs_Atoms.contains(atom)) {
      return Seq()
    }
    else {
      val vn = VNs_Atoms.getIdOf(atom)
      return Seq(atom)
    }
  }

}
