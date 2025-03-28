package inca.ir.extension.record

import inca.ir.{Arg, Atom, BaseIR, Language, ModuleEntry, Name, Ref, RefByName, Term, Type, Var}
import inca.ir.extension.data
import inca.ir.extension.block

// TODO: Souffle Records are different, since they automatically support a nil case for records, we do not
// TODO: Support typeparam IR
object IR extends IR {}

trait IR extends BaseIR:
  override val name: String = "Record"
  override def language: Language = super.language + IR
  override def requires: Language = Language(data.IR, block.IR)

case class TRecord(ref: Ref[RecordDefinition]) extends Type:
  override def toString: String = s"$ref"

object TRecord:
  def apply(name: Name) = new TRecord(RefByName(name))

trait RecordModuleEntry extends ModuleEntry

case class RecordDefinition(name: Name) extends RecordModuleEntry:
  override def toString: String = s"""record $name"""
  def withName(name: String): RecordDefinition = this.copy(name = Name(name))

case class FieldDefinition(fieldName: Name, ty: Type, record: TRecord) extends RecordModuleEntry:
  override def toString: String = s"""field $name: $ty"""
  override val name: Name = Name(s"${record.ref.name}.$fieldName")
  def withName(name: String): FieldDefinition = this.copy(fieldName = Name(name))

case class RecordLit(name: Ref[RecordDefinition], fields: Seq[(Ref[FieldDefinition], Term)]) extends Term:
  override def toString: String = s"""$name${fields.mkString("(", ", ", ")")}"""
  override def vars: Seq[Var] = fields.flatMap { case (name, term) => term.vars }
  override def commonVars: Set[Var] = fields.flatMap { case (name, term) => term.commonVars }.toSet

object RecordLit:
  def apply(name: Name, fields: Seq[(Ref[FieldDefinition], Term)]) = new RecordLit(RefByName(name), fields)

case class FieldLookup(record: Term, field: Ref[FieldDefinition]) extends Term:
  override def toString: String = s"""$record.$field"""
  def vars: Seq[Var] = record.vars
  override def commonVars: Set[Var] = record.commonVars

object FieldLookup:
  def apply(record: Term, field: Name) = new FieldLookup(record, RefByName(field))

case class Deconstruct(record: Term, name: Ref[RecordDefinition], fields: Seq[(Name, Arg)], neg: Boolean = false) extends Atom:
  override def toString: String = s"""${if !neg then "" else "~"} ?$name${fields.mkString("(", ", ", ")")}"""
  def vars: Seq[Var] = record.vars ++ fields.flatMap { case (name, arg) => arg.vars }
  override def commonVars: Set[Var] = record.commonVars ++ fields.flatMap { case (name, arg) => arg.commonVars }
