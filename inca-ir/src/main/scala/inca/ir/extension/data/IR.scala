package inca.ir.extension.data

import inca.ir.*

import scala.language.implicitConversions

trait DataDefinitionBase extends DataModuleEntry

trait CaseDefinitionBase extends DataModuleEntry:
  def data: TData
  def args: Seq[Type]

case class DataDefinitionImport(name: Name) extends ModuleImport with DataDefinitionBase:
  def withExtendedName(suffix: String): DataDefinitionImport = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""import data $name"""

case class CaseDefinitionImport(name: Name, args: Seq[Type], data: TData) extends ModuleImport with CaseDefinitionBase:
  def withExtendedName(suffix: String): CaseDefinitionImport = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""import case $name(${args.mkString(",")}): $data"""

case class DataDefinitionExport(name: Name) extends ModuleExport:
  def withExtendedName(suffix: String): DataDefinitionExport = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""export data $name"""

case class CaseDefinitionExport(name: Name, args: Seq[Type], data: TData) extends ModuleExport:
  def withExtendedName(suffix: String): CaseDefinitionExport = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""export case $name(${args.mkString(",")}): $data"""

case class TData(ref: Ref[DataDefinitionBase]) extends Type:
  override def toString: String = s"$ref"
object TData:
  def apply(name: Name) = new TData(RefByName(name))

trait DataModuleEntry extends ModuleEntry

case class DataDefinition(name: Name) extends DataDefinitionBase:
  def withExtendedName(suffix: String): DataDefinition = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""data $name"""

case class CaseDefinition(name: Name, args: Seq[Type], data: TData) extends CaseDefinitionBase:
  def withExtendedName(suffix: String): CaseDefinition = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""case $name(${args.mkString(",")}): $data"""

case class Construct(caseRef: Ref[CaseDefinitionBase], args: Seq[Term]) extends Term:
  override def toString: String = s"!$caseRef(${args.mkString(", ")})" + analysisString
  override def vars: Seq[Var] = args.flatMap(_.vars)
object Construct:
  def apply(caseName: Name, args: Seq[Term]): Construct = new Construct(RefByName(caseName), args)

case class Deconstruct(t: Term, caseRef: Ref[CaseDefinitionBase], args: Seq[Arg], neg: Boolean) extends Atom:
  override def toString: String =
    val ifArgs = if (args.isEmpty) "" else ", "
    val negPrefix = if (neg) "~" else ""
    s"$negPrefix?$caseRef($t$ifArgs${args.mkString(", ")})" + analysisString
  override def vars: Seq[Var] = t.vars ++ args.flatMap(_.vars)
object Deconstruct:
  def apply(t: Term, caseName: Name, args: Seq[Arg], neg: Boolean = false): Deconstruct =
    new Deconstruct(t, RefByName(caseName), args, neg)

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Data"
  override def language: Language = super.language + IR
  override def requires: Language = Language()
