package inca.ir.extension.record

import inca.ir.Hint.preserveHints
import inca.ir.{Atom, ModuleEntry, Term, Type}
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor {

  override def visitType(ty: Type): Type = ty match {
    case TRecord(name) => TRecord(name)
    case _ => super.visitType(ty)
  }

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match {
    case RecordDefinition(name) => Seq(RecordDefinition(name))
    case FieldDefinition(name, ty, record) => Seq(FieldDefinition(name, visitType(ty), visitType(record).asInstanceOf[TRecord]))
    case _ => super.visitModuleEntry(moduleEntry)
  })

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match {
    case RecordLit(name, fields) =>
      val newFields = fields.flatMap { (name, term) => visitTerm(term).map((name, _)) }
      Seq(RecordLit(name, newFields))
    case FieldLookup(record, field) => visitTerm(record).map(FieldLookup(_, field))
    case _ => super.visitTerm(term)
  })

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match {
    case Deconstruct(record, name, fields, neg) =>
      val records = visitTerm(record)
      val aargs = fields.flatMap { case (name, arg) => visitArg(arg).map((name, _)) }
      records.map(Deconstruct(_, name, aargs, neg))
    case _ => super.visitAtom(atom)
  })

}
