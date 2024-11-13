package inca.ir.extension.datamatch

import inca.ir.{Atom, BaseIR, Language, Name, Ref, Term, Var}
import inca.ir.extension.data
import inca.ir.extension.data.CaseDefinition
import inca.ir.extension.disjunction
import inca.ir.util.SourceLocation

// TODO: Support wildcards here ?
case class Case(name: Ref[CaseDefinition], patVars: Seq[Var], body: Seq[Atom]) extends SourceLocation:
  override def toString: String = s"case $name(${patVars.mkString(", ")}) => ${body.mkString(", ")}"

  def vars: Seq[Var] = patVars.flatMap(_.vars) ++ body.flatMap(_.vars)

case class Match(matchee: Term, cases: Seq[Case]) extends Atom:
  override def toString: String = s"$matchee match ${cases.mkString("\n\t\t", "\n\t\t", "\n")}"

  override def vars: Seq[Var] = matchee.vars ++ cases.flatMap(_.vars)

object IR extends IR {}

trait IR extends data.IR with disjunction.IR:
  override val name: String = "DataMatch"

  override def language: Language = super.language + IR

  override def requires: Language = Language()
