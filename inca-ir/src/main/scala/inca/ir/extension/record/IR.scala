package inca.ir.extension.record

import inca.ir.{Arg, Atom, BaseIR, Language, ModuleEntry, RefByName, Name, Ref, Term, Type}
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
case class RecordDefinition(name: Name) extends RecordModuleEntry{
  def withExtendedName(suffix: String): RecordDefinition = this.copy(name = Name(name.name + suffix))
  override def toString: String = s"""record $name"""
}

case class FieldDefinition(fieldName: Name, ty: Type, record: TRecord) extends RecordModuleEntry:
  override val name: Name = Name(s"${record.ref.name}.$fieldName")
  def withExtendedName(suffix: String): inca.ir.ModuleEntry = this.copy(fieldName = Name(name.name + suffix)) 

  override def toString: String = s"""field $name: $ty"""

case class RecordLit(name: Ref[RecordDefinition], fields: Seq[(Ref[FieldDefinition], Term)]) extends Term:
  override def toString: String = s"""$name${fields.mkString("(", ", ", ")")}"""
  def vars: Seq[inca.ir.Var] = fields.flatMap{ case (name, term) => term.vars }
object RecordLit:
  def apply(name: Name, fields: Seq[(Ref[FieldDefinition], Term)]) = new RecordLit(RefByName(name), fields)

case class FieldLookup(record: Term, field: Ref[FieldDefinition]) extends Term:
  override def toString: String = s"""$record.$field"""
  def vars: Seq[inca.ir.Var] = record.vars
object FieldLookup:
  def apply(record: Term, field: Name) = new FieldLookup(record, RefByName(field))

case class Deconstruct(record: Term, name: Ref[RecordDefinition], fields: Seq[(Name, Arg)], neg: Boolean = false) extends Atom:
  override def toString: String = s"""${if !neg then "" else "~"} ?$name${fields.mkString("(", ", ", ")")}"""
  def vars: Seq[inca.ir.Var] = record.vars ++ fields.flatMap{ case (name, arg) => arg.vars }
