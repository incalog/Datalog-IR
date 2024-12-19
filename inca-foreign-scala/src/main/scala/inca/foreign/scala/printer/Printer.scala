package inca.foreign.scala.printer

import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaConstantTerm, ScalaDefnModuleEntry, ScalaTerm, ScalaType}
import inca.ir.{Arg, Atom, ModuleEntry, Term, Type}
import inca.ir.printer.BaseIRPrinter
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.AggregationOperator

trait Printer
  extends BaseIRPrinter
  with aggregate.printer.Printer:

  override def prettyPrint(op: AggregationOperator): String = op match
    case ScalaAggregationOperator(name, ty, initCode, addCode) => s"$name -> $ty"
    case _ => super.prettyPrint(op)

  override def prettyPrint(moduleEntry: ModuleEntry): String = moduleEntry match
    case ScalaDefnModuleEntry(name, code) => code
    case _ => super.prettyPrint(moduleEntry)

  override def prettyPrint(ty: Type): String = ty match
    case ScalaType(ty) => s"`${ty}`" 
    case _ => super.prettyPrint(ty)

  override def prettyPrint(term: Term): String = term match
    case ScalaTerm(code, ty, args, true) => s"`($code)(${args.map(prettyPrint).mkString(", ")})`"
    case ScalaTerm(code, ty, args, false) => s"`$code`"
    case ScalaConstantTerm(code, ty) => s"`$code`"
    case _ => super.prettyPrint(term)
