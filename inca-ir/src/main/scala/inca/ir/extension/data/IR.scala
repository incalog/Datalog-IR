package inca.ir.extension.data

import inca.ir.*

import scala.language.implicitConversions

case class TData(name: Name) extends Type:
  override def toString: String = s"$name"

case class CaseDefinition(name: Name, args: Seq[Type]):
  override def toString: String = s"""$name(${args.mkString(",")})"""

case class DataDefinition(name: Name, cases: Seq[CaseDefinition]) extends ModuleEntry:
  override def toString: String = s"""data $name = ${cases.mkString(" | ")}"""

case class Construct(name: Name, args: Seq[Term]) extends Term:
  override def toString: String = s"$name${args.mkString("(", ", ", ")")}"
  override def vars: Seq[Var] = args.flatMap(_.vars)

case class Case(name: Name, args: Seq[Term], body: Seq[Atom]):
  override def toString: String = s"case $name(${args.mkString(", ")}) => ${body.mkString(", ")}"
  def vars: Seq[Var] = args.flatMap(_.vars) ++ body.flatMap(_.vars)

// We could lower this to disjunctionIR first
case class Match(matchee: Term, cases: Seq[Case]) extends Atom:
  override def toString: String = s"$matchee match ${cases.mkString("\n\t\t", "\n\t\t", "\n")}"
  override def vars: Seq[Var] = matchee.vars ++ cases.flatMap(_.vars)


object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + IR
  override def requires: Language = Language()
