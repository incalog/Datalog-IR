package inca.ir.extension.edbdata.printer

import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.edbdata.{Link, UndefEdbFieldInverse, UndefEdbField, LookupEdbField, UndefEdbType, LookupEdbType, NotInEdbType, EdbFieldDefinition, EdbNodeDefinition, TEdbList, TEdbNode, TEdbValue}
import inca.ir.{Atom, ModuleEntry, Name, Term, Type}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  def prettyPrint(link: Link): String = link match
    case Link.Field(name) => name.toString
    case _ => super.toString

  override def prettyPrint(moduleEntry: ModuleEntry): String = moduleEntry match
    case EdbNodeDefinition(name, sup) =>
      s"edb node ${prettyPrint(name)}" + sup.map(prettyPrint).map(" extends " + _).getOrElse("")
    case EdbFieldDefinition(node, field, ty) =>
      s"""edb field ${prettyPrint(node)}.${prettyPrint(field)}: ${prettyPrint(ty)}"""
    case _ => super.prettyPrint(moduleEntry)

  override def prettyPrint(ty: Type): String = ty match
    case TEdbValue(ty) => s"${prettyPrint(ty)}@edb"
    case TEdbNode(name) => s"${prettyPrint(name)}@edb"
    case TEdbList(ty) => s"List[${prettyPrint(ty)}]@edb"
    case _ => super.prettyPrint(ty)

  override def prettyPrint(term: Term): String = term match
    case LookupEdbType(ty) => s"edb[${prettyPrint(ty)}]"
    case LookupEdbField(src, link) => s"(${prettyPrint(src)}).${prettyPrint(link)}"
    case _ => super.prettyPrint(term)

  override def prettyPrint(atom: Atom): String = atom match
    case NotInEdbType(t, ty) => s"not ${prettyPrint(t)} in edb[${prettyPrint(ty)}]"
    case UndefEdbType(ty) => s"undef edb[${prettyPrint(ty)}]"
    case UndefEdbField(src, link) => s"undef ${prettyPrint(src)}.${prettyPrint(link)}"
    case UndefEdbFieldInverse(srcTy, link, trg) => s"undef ${prettyPrint(trg)}.${prettyPrint(link)}^⁻¹"
    case _ => super.prettyPrint(atom)
