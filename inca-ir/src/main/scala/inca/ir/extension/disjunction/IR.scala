package inca.ir.extension.disjunction

import inca.ir.util.SourceLocation
import inca.ir.{Atom, BaseIR, Body, Language, Var}

import scala.annotation.tailrec

object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "Disjunction"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class DisjunctionAlternative(body: Body) extends SourceLocation

object DisjunctionAlternative:
  def apply(at: Atom): DisjunctionAlternative = DisjunctionAlternative(Body(Seq(at)))
  def apply(ats: Seq[Atom]): DisjunctionAlternative = DisjunctionAlternative(Body(ats))
  def apply(at: Atom, ats: Atom*): DisjunctionAlternative = DisjunctionAlternative(Body(at +: ats))

case class Disjunction(alternatives: Seq[DisjunctionAlternative]) extends Atom:
  override def vars: Seq[Var] = alternatives.flatMap(_.body.atoms.flatMap(_.vars))

object Disjunction:
  def apply(b1: Body, b2: Body): Disjunction =
    Disjunction(Seq(DisjunctionAlternative(b1), DisjunctionAlternative(b2)))

  def apply(as1: Seq[Atom], as2: Seq[Atom]): Disjunction =
    Disjunction(Seq(DisjunctionAlternative(as1), DisjunctionAlternative(as2)))

