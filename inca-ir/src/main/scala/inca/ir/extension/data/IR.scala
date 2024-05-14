package inca.ir.extension.data

import inca.ir.*

import scala.language.implicitConversions

case class TData(ref: Ref[DataDefinition]) extends Type:
  override def toString: String = s"$ref"
object TData:
  def apply(name: Name) = new TData(RefByName(name))

trait DataModuleEntry extends ModuleEntry

case class DataDefinition(name: Name) extends DataModuleEntry:
  def withName(name: String): DataDefinition = this.copy(name = Name(name))
  override def toString: String = s"""data $name"""

case class CaseDefinition(name: Name, args: Seq[Type], data: TData) extends DataModuleEntry:
  def withName(name: String): CaseDefinition = this.copy(name = Name(name))
  override def toString: String = s"""case $name(${args.mkString(",")}): $data"""

case class Construct(caseRef: Ref[CaseDefinition], args: Seq[Term]) extends Term:
  override def toString: String = s"!$caseRef(${args.mkString(", ")})" + analysisString
  override def vars: Seq[Var] = args.flatMap(_.vars)
object Construct:
  def apply(caseName: Name, args: Seq[Term]): Construct = new Construct(RefByName(caseName), args)

case class Deconstruct(t: Term, caseRef: Ref[CaseDefinition], args: Seq[Arg], neg: Boolean) extends Atom:
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
