package inca.ir.extension.string

import inca.ir.*
import inca.ir.extension.block
import inca.ir.extension.bool


case object TString extends Type

case class StringLit(value: String) extends Term:
  override def toString: String = s"\"$value\""

case class StringConcat(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs + $rhs"
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "String"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)