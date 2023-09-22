package inca.ir.extension.disjunction

import inca.ir.{Atom, BaseIR, Language, Var}

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Disjunction"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class Disjunction(alternatives: Seq[Seq[Atom]]) extends Atom:
  override def toString: String =
    alternatives.map(_.mkString("{", ", ", "}")).mkString(" or ")
  override def vars: Seq[Var] = alternatives.flatMap(_.flatMap(_.vars))

object Disjunction:
  def apply(as1: Seq[Atom], as2: Seq[Atom]): Disjunction = Disjunction(Seq(as1, as2))

