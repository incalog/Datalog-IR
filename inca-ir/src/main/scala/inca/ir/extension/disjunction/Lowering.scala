package inca.ir.extension.disjunction

import inca.ir.Hint.preserveHints
import inca.ir.extension.disjunction.{Disjunction, IR}
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body}

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  type Alternatives[A] = Seq[A]
  private var alternativeAtoms: Alternatives[Seq[Atom]] = Seq()

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    alternativeAtoms = Seq(Seq())
    super.visitBody(body)
    alternativeAtoms.map(Body.apply)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Disjunction(ass) =>
        val before = alternativeAtoms

        val alternatives: ListBuffer[Seq[Atom]] = ListBuffer.empty
        val rss = ass.map { as =>
          val rs = as.flatMap(visitAtom)
          alternatives ++= alternativeAtoms
          alternativeAtoms = before
          rs
        }

        alternativeAtoms = alternatives.toSeq
        Seq(Disjunction(rss))
      case _ =>
        val as = super.visitAtom(atom)
        alternativeAtoms = alternativeAtoms.map(_ ++ as)
        as
  }
