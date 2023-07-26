package inca.ir.extension.data

import inca.ir.*

import scala.language.implicitConversions

case class TData(name: Name) extends Type:
  override def toString: String = s"$name"

case class CaseDefinition(name: Name, args: Seq[Type]):
  override def toString: String = s"""$name ${args.mkString(" ")}"""

case class DataDefinition(override val name: Name, cases: Seq[CaseDefinition]) extends ModuleEntry(name):
  override def toString: String = s"""data $name = ${cases.mkString("|")}"""

case class Construct(name: Name, data: Seq[Term]) extends Term

case class Case(name: Name, vars: Seq[Var], body: Seq[Atom]) // TODO Discuss: or Body or Term ?
// We could lower this to disjunctionIR first
case class Match(matchee: Term, cases: Seq[Case]) extends Atom


object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + new IR {}
  override def requires: Language = Language()
