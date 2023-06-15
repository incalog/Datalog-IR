package inca.ir.extensions

import inca.ir.{Atom, BaseIR, Language}

case class Disjunction(as1: Seq[Atom], as2: Seq[Atom]) extends Atom:
  override def toString: String = s"${as1.mkString("(", ", ", ")")} v ${as2.mkString("(", ", ", ")")}"

object DisjunctionIR extends DisjunctionIR { }
trait DisjunctionIR extends BaseIR:
  override val name: String = "Disjunction"
  override def language: Language = super.language + DisjunctionIR
  override def requires: Language = Language()
