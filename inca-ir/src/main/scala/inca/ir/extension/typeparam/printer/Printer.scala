package inca.ir.extension.typeparam.printer

import inca.ir.{ModuleEntry, Ref, Type}
import inca.ir.extension.typeparam.{TypeVar, ParametricModuleEntry, TypeApplication}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  override def prettyPrint(moduleEntry: ModuleEntry): String = moduleEntry match
    case ParametricModuleEntry(typeParams, entry) =>
      s"with[${typeParams.map(prettyPrint).mkString(", ")}] ${prettyPrint(entry)}"
    case _ => super.prettyPrint(moduleEntry)

  override def prettyPrint(ref: Ref[?]): String = ref match
    case TypeApplication(name, args) => s"${prettyPrint(name)}[${args.map(prettyPrint).mkString(", ")}]"
    case _ => super.prettyPrint(ref)

  override def prettyPrint(ty: Type): String = ty match
    case TypeVar(name) => prettyPrint(name)
    case _ => super.prettyPrint(ty)


