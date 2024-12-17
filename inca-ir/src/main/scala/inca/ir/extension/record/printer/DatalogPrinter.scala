package inca.ir.extension.record.printer

import inca.ir.{Atom, ModuleEntry, Term, Type}
import inca.ir.extension.record.{RecordDefinition, Deconstruct, FieldLookup, FieldDefinition, RecordLit, TRecord}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(moduleEntry: ModuleEntry): String = moduleEntry match
    case RecordDefinition(name) => s"record ${prettyPrint(name)}"
    case FieldDefinition(name, ty, record) => s"field ${prettyPrint(name)}: ${prettyPrint(ty)}"
    case _ => super.prettyPrint(moduleEntry)

  override def prettyPrint(atom: Atom): String = atom match
    case Deconstruct(record, name, fields, neg) =>
      val prefix = if !neg then "" else "~"
      val fieldS = fields.map((f, t) => s"${prettyPrint(f)} -> ${prettyPrint(t)}")
      s"$prefix?${prettyPrint(name)}${fieldS.mkString("(", ", ", ")")}"
    case _ => super.prettyPrint(atom)

  override def prettyPrint(term: Term): String = term match
    case RecordLit(name, fields) =>
      s"${prettyPrint(name)}${fields.map((f, t) => s"${prettyPrint(f)} -> ${prettyPrint(t)}").mkString("(", ", ", ")")}"
    case FieldLookup(record, field) =>
      s"${prettyPrint(record)}.${prettyPrint(field)}"
    case _ => super.prettyPrint(term)

  override def prettyPrint(ty: Type): String = ty match
    case TRecord(ref) => prettyPrint(ref)
    case _ => super.prettyPrint(ty)


