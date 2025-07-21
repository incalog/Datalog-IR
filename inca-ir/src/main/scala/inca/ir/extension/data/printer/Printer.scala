package inca.ir.extension.data.printer

import inca.ir.extension.bool.{AtomAsBool, BoolAnd, BoolFalse, BoolNot, BoolOr, BoolTerm, BoolTrue, TBoolean}
import inca.ir.extension.data.{TData, Construct, Deconstruct, DataDefinition, CaseDefinition, DataDefinitionSubstitution, CaseDefinitionSubstitution, ProvideCaseDefinition, ProvideDataDefinition, RequireCaseDefinition, RequireDataDefinition}
import inca.ir.{Arg, Atom, ModuleEntry, Substitution, Term, Type}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  override def prettyPrint(moduleEntry: ModuleEntry): String = moduleEntry match
    case DataDefinition(name) => s"data ${prettyPrint(name)}"
    case CaseDefinition(name, args, data) => s"case ${prettyPrint(name)}(${args.map(prettyPrint).mkString(",")}): ${prettyPrint(data)}"
    case RequireDataDefinition(name) => s"require data ${prettyPrint(name)}"
    case RequireCaseDefinition(name, args, data) => s"require case ${prettyPrint(name)}(${args.map(prettyPrint).mkString(", ")})"
    case ProvideDataDefinition(exportRef) => s"provide data ${prettyPrint(exportRef)}"
    case ProvideCaseDefinition(exportRef, args, data) => s"provide case ${prettyPrint(exportRef)}(${args.map(prettyPrint).mkString(", ")})"
    case _ => super.prettyPrint(moduleEntry)

  override def prettyPrint(subst: Substitution[?, ?]): String = subst match
    case DataDefinitionSubstitution(to, from) => s"data ${prettyPrint(to)} = data ${prettyPrint(from)}"
    case CaseDefinitionSubstitution(to, toSig, from, fromSig) =>
      s"case ${prettyPrint(to)}(${toSig.map(prettyPrint).mkString(", ")}) = case ${prettyPrint(from)}(${fromSig.map(prettyPrint).mkString(", ")})"
    case _ => super.prettyPrint(subst)
    
  override def prettyPrint(ty: Type): String = ty match
    case TData(ref) => prettyPrint(ref)
    case _ => super.prettyPrint(ty)

  override def prettyPrint(atom: Atom): String = atom match
    case Deconstruct(t, caseRef, args, neg) =>
      val ifArgs = if (args.isEmpty) "" else ", "
      val negPrefix = if (neg) "~" else ""
      s"$negPrefix?${prettyPrint(caseRef)}(${prettyPrint(t)}$ifArgs${args.map(prettyPrint).mkString(", ")})"
    case _ => super.prettyPrint(atom)
  
  override def prettyPrint(term: Term): String = term match
    case Construct(caseRef, args) => s"!${prettyPrint(caseRef)}(${args.map(prettyPrint).mkString(", ")})"
    case _ => super.prettyPrint(term)
