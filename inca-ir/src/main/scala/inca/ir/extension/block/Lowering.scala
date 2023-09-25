package inca.ir.extension.block

import inca.ir.Hint.preserveHints
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Term}

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:

  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  private var embeddedAtoms: List[Atom] = List()

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    val before = embeddedAtoms
    embeddedAtoms = List()
    val as = super.visitAtom(atom)
    val after = embeddedAtoms
    embeddedAtoms = before
    after ++ as
  }

  override def visitTerm(term: Term): Seq[Term] =  preserveHints(term)(term match
    case Block(as, t) =>
      embeddedAtoms ++= as.flatMap(visitAtom)
      visitTerm(t)
    case _ => super.visitTerm(term))
  