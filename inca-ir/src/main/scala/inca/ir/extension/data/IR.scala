package inca.ir.extension.data

import inca.ir.*

import scala.language.implicitConversions

case class TData(ref: Ref[DataDefinitionReference]) extends Type:
  override def toString: String = s"$ref"
object TData:
  def apply(name: Name) = new TData(RefByName(name))
  def apply(names: Seq[Name]): TData =
    if names.size == 1 then
      new TData(RefByName(names.last))
    else
      new TData(RefByQualifiedName(names))


trait DataModuleEntry extends ModuleEntry
trait DataDefinitionReference extends DataModuleEntry
trait CaseDefinitionReference extends DataModuleEntry:
  def args: Seq[Type]
  def data: TData

// extend the module system

trait DataDefinitionProvidable extends DataModuleEntry, Providable
trait CaseDefinitionProvidable extends DataModuleEntry, Providable

case class RequireDataDefinition(name: Name) extends DataDefinitionProvidable, Require:
  override def toString: String = s"require data $name"
  def withName(name: String): ModuleEntry = this.copy(name = Name(name))

case class RequireCaseDefinition(name: Name, args: Seq[Type], data: TData) extends CaseDefinitionReference, CaseDefinitionProvidable, Require:
  override def toString: String = s"require case $name(${args.mkString(", ")})"
  def withName(name: String): ModuleEntry = this.copy(name = Name(name))

case class ProvideDataDefinition(exportRef: Ref[DataDefinitionProvidable]) extends DataDefinitionReference, Provide[DataDefinitionProvidable]:
  override def toString: String = s"provide data $exportRef"
  def withName(name: String): ModuleEntry =
    val newRef = RefByName[DataDefinitionProvidable](name)
    newRef.target = exportRef.target
    this.copy(exportRef = newRef)
object ProvideDataDefinition:
  def apply(exportName: Name) =
    new ProvideDataDefinition(RefByName(exportName))

case class ProvideCaseDefinition(exportRef: Ref[CaseDefinitionProvidable], args: Seq[Type], data: TData) extends CaseDefinitionReference, Provide[CaseDefinitionProvidable]:
  override def toString: String = s"provide case $exportRef(${args.mkString(", ")})"
  def withName(name: String): ModuleEntry =
    val newRef = RefByName[CaseDefinitionProvidable](name)
    newRef.target = exportRef.target
    this.copy(exportRef = newRef)
object ProvideCaseDefinition:
  def apply(exportName: Name, args: Seq[Type], data: TData) =
    new ProvideCaseDefinition(RefByName(exportName), args, data)

case class CaseDefinitionSubstitution(to: Ref[RequireCaseDefinition], toSig: Seq[Type], from: Ref[CaseDefinitionProvidable], fromSig: Seq[Type]) extends Substitution[RequireCaseDefinition, CaseDefinitionProvidable]:
  override def toString: String = s"case $to(${toSig.mkString(", ")}) = case ${from.name}(${fromSig.mkString(", ")})"
object CaseDefinitionSubstitution:
  def apply(to: Name, toSig: Seq[Type], from: Seq[Name], fromSig: Seq[Type]): CaseDefinitionSubstitution =
    if from.isEmpty then
      throw IllegalStateException("Path to a case definition must not be empty")
    new CaseDefinitionSubstitution(RefByName(to), fromSig, RefByQualifiedName(from), toSig)

case class DataDefinitionSubstitution(to: Ref[RequireDataDefinition], from: Ref[DataDefinitionProvidable]) extends Substitution[RequireDataDefinition, DataDefinitionProvidable]:
  override def toString: String = s"data $to = data ${from.name}"
object DataDefinitionSubstitution:
  def apply(to: Name, from: Seq[Name]): DataDefinitionSubstitution =
    if from.isEmpty then
      throw IllegalStateException("Path to a data definition must not be empty")
    new DataDefinitionSubstitution(RefByName(to), RefByQualifiedName(from))


// IR

case class DataDefinition(name: Name) extends DataDefinitionReference, DataDefinitionProvidable:
  def withName(name: String): DataDefinition = this.copy(name = Name(name))
  override def toString: String = s"""data $name"""

case class CaseDefinition(name: Name, args: Seq[Type], data: TData) extends CaseDefinitionReference, CaseDefinitionProvidable:
  def withName(name: String): CaseDefinition = this.copy(name = Name(name))
  override def toString: String = s"""case $name(${args.mkString(",")}): $data"""

case class Construct(caseRef: Ref[_ <: CaseDefinitionReference], args: Seq[Term]) extends Term:
  override def toString: String = s"!$caseRef(${args.mkString(", ")})" + analysisString
  override def vars: Seq[Var] = args.flatMap(_.vars)
object Construct:
  def apply(caseName: Name, args: Seq[Term]): Construct = new Construct(RefByName(caseName), args)
  def apply(caseName: Seq[Name], args: Seq[Term]): Construct =
    if caseName.size == 1 then
      new Construct(RefByName(caseName.last), args)
    else
      new Construct(RefByQualifiedName(caseName), args)

case class Deconstruct(t: Term, caseRef: Ref[_ <: CaseDefinitionReference], args: Seq[Arg], neg: Boolean) extends Atom:
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
