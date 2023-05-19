package inca.ir.lowering

import inca.ir.{Atom, Body, Call, IR, IRVisitor, Module}
import inca.ir.extensions.{Disjunction, DisjunctionIR}

trait Lowering[S <: DisjunctionIR, T <: IR](val src: S, val trg: T) extends IRVisitor {
  def loweredIRs: Set[IR] = Set()

  def lower(module: Module): Module = {
    assert(module.lang.includes(trg.requires))
    visit(Module(module.name, module.lang -- loweredIRs, module.contents))
  }
}

case class DisjunctionLowering[S <: DisjunctionIR, T <: IR](override val src: S, override val trg: T) extends Lowering[S, T](src, trg) {
  override def loweredIRs: Set[IR] = Set(new DisjunctionIR {})

  type Alternatives[A] = Seq[A]
  private var alternativeAtoms: Alternatives[Seq[Atom]] = Seq()

  override def visitBody(body: Body): Seq[Body] = {
    alternativeAtoms = Seq(Seq())
    super.visitBody(body)
    alternativeAtoms.map(Body.apply)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Disjunction(as1, as2) =>
      val before = alternativeAtoms
      val lhs = as1.flatMap(visitAtom)
      val lhsAtoms = alternativeAtoms

      alternativeAtoms = before
      val rhs = as2.flatMap(visitAtom)
      val rhsAtoms = alternativeAtoms

      alternativeAtoms = lhsAtoms ++ rhsAtoms
      Seq(Disjunction(lhs, rhs))
    case _ =>
      val a = super.visitAtom(atom)
      alternativeAtoms = alternativeAtoms.map(_.appendedAll(a))
      a
}
