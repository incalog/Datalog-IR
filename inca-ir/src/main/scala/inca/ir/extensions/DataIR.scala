package inca.ir.extensions
import scala.language.implicitConversions
import inca.ir.{Atom, BaseIR, Body, Language, ModuleEntry, Name, Term, Type, Var}

case class TData(name: Name) extends Type:
  override def toString: String = s"$name"

case class CaseDefinition(name: Name, args: Seq[Type]):
  override def toString: String = s"""$name ${args.mkString(" ")}"""

case class DataDefinition(name: Name, cases: Seq[CaseDefinition]) extends ModuleEntry:
  //private lazy val caseMap: Map[Name, Case] = cases.map(c => c.name -> c).toMap
  //def getCaseByName(name: Name): Option[Case] = caseMap.get(name)
  override def toString: String = s"""data $name = ${cases.mkString("|")}"""

case class Construct(name: Name, data: Seq[Term]) extends Term

case class Case(name: Name, vars: Seq[Var], body: Term) // TODO Discuss: Seq[Atom] or Body ?
case class Match(data: Term, cases: Seq[Case]) extends Atom


trait DataIR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + new DataIR {}
  override def requires: Language = Language()
