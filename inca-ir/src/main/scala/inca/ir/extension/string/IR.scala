package inca.ir.extension.string

import inca.ir.*
import inca.ir.extension.block
import inca.ir.extension.bool


case object TString extends Type

case class StringLit(value: String) extends Term:
  override def toString: String = s"\"$value\""
  override def vars: Seq[Var] = Seq()
  override def commonVars: Set[Var] = Set()

case class StringConcat(lhs: Term, rhs: Term) extends Term:
  override def toString: String = s"$lhs + $rhs"
  override def vars: Seq[Var] = lhs.vars ++ rhs.vars
  override def commonVars: Set[Var] = lhs.commonVars ++ rhs.commonVars

case class ToString(t: Term) extends Term:
  override def toString: String = s"$t.toString"
  override def vars: Seq[Var] = t.vars
  override def commonVars: Set[Var] = t.commonVars

case class Substring(t: Term, index: Term, length: Term) extends Term:
  override def toString: String = s"$t[$index..<$length]"
  override def vars: Seq[Var] = t.vars ++ index.vars ++ length.vars
  override def commonVars: Set[Var] = t.commonVars ++ index.commonVars ++ length.commonVars

case class OrdinalNumber(t: Term) extends Term:
  override def toString: String = s"ord($t)"
  override def vars: Seq[Var] = t.vars
  override def commonVars: Set[Var] = t.commonVars

case class RegexMatch(t: Term, pattern: Term, neg: Boolean = false) extends Atom:
  override def toString: String =
    val negPrefix = if (neg) "~" else ""
    s"${negPrefix}reg_match($t, $pattern)"
  override def vars: Seq[Var] = t.vars ++ pattern.vars
  override def commonVars: Set[Var] = t.commonVars ++ pattern.commonVars

case class StringLength(t: Term) extends Term:
    override def toString: String = s"$t.length"
    override def vars: Seq[Var] = t.vars
    override def commonVars: Set[Var] = t.commonVars


object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "String"
  override def language: Language = super.language + IR
  override def requires: Language = Language(IR)