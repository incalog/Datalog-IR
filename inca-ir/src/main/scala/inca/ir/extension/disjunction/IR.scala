package inca.ir.extension.disjunction

import inca.ir.util.SourceLocation
import inca.ir.{Atom, BaseIR, Language, Var}

import scala.annotation.tailrec

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Disjunction"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class DisjunctionAlternative(atoms: Seq[Atom]) extends SourceLocation:
  override def toString: String = atoms.mkString("{", ", ", "}")
object DisjunctionAlternative:
  def apply(at: Atom): DisjunctionAlternative = DisjunctionAlternative(Seq(at))
  def apply(at: Atom, ats: Atom*): DisjunctionAlternative = DisjunctionAlternative(at +: ats)
case class Disjunction(alternatives: Seq[DisjunctionAlternative]) extends Atom:
  override def toString: String =
    alternatives.mkString(" or ")
  override def vars: Seq[Var] = alternatives.flatMap(_.atoms.flatMap(_.vars))

object Disjunction:
  def apply(as1: Seq[Atom], as2: Seq[Atom]): Disjunction =
    Disjunction(Seq(DisjunctionAlternative(as1), DisjunctionAlternative(as2)))

