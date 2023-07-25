package inca.ir.extension.block

import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Term}

import scala.collection.mutable.ListBuffer

object Lowering:
  def apply[S <: IR, T <: BaseIR](srcIR: S, trgIR: T): Lowering[S, T] = new Lowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

trait Lowering[S <: IR, T <: BaseIR] extends BaseLowering[S, T]:

  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  private var embeddedAtoms: List[Atom] = List()

  override def visitAtom(atom: Atom): Seq[Atom] =
    val before = embeddedAtoms
    embeddedAtoms = List()
    val as = super.visitAtom(atom)
    val after = embeddedAtoms
    embeddedAtoms = before
    after ++ as

  override def visitTerm(term: Term): Seq[Term] = term match
    case Block(as, t) =>
      embeddedAtoms ++= as.flatMap(visitAtom)
      visitTerm(t)
    case _ => super.visitTerm(term)
  