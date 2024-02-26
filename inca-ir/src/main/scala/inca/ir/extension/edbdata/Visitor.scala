package inca.ir.extension.edbdata

import inca.ir.{Atom, ModuleEntry, Term, Type}
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor:

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case EdbNodeDefinition(name, sup) => Seq(EdbNodeDefinition(name, sup))
    case EdbFieldDefinition(node, field, ty) => Seq(EdbFieldDefinition(node, field, visitEdbType(ty)))
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitType(ty: Type): Type = ty match
    case ety: EdbType => visitEdbType(ety)
    case _ => super.visitType(ty)

  def visitEdbType(ety: EdbType): EdbType = ety match
    case TEdbValue(ty) => TEdbValue(ty)
    case TEdbNode(name) => TEdbNode(name)
    case TEdbList(ty) => TEdbList(visitEdbType(ty))

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case NotInEdbType(t, ety) => visitTerm(t).map(NotInEdbType(_, visitEdbType(ety)))
    case UndefEdbType(ety) => Seq(UndefEdbType(visitEdbType(ety)))
    case UndefEdbField(t, link) => visitTerm(t).map(UndefEdbField(_, link))
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case LookupEdbType(ety) => Seq(LookupEdbType(visitEdbType(ety)))
    case LookupEdbField(t, link) => visitTerm(t).map(LookupEdbField(_, link))
    case _ => super.visitTerm(term)
  