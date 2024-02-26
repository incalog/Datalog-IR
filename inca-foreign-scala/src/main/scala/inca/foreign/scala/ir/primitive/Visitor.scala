package inca.foreign.scala.ir.primitive

import inca.ir.{Atom, BaseIR, ModuleEntry, Term, Type}
import inca.ir.visitors.BaseIRVisitor
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.Aggregate
import inca.ir.extension.mono.MonoAggregationOperator

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor with aggregate.Visitor:
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case ScalaDefnModuleEntry(name, defn) => Seq(ScalaDefnModuleEntry(name, defn))
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitAggregationOperator(op: aggregate.AggregationOperator): aggregate.AggregationOperator = op match
    case ScalaAggregationOperator(name, ty, initCode, addCode) =>
      val vty = visitType(ty)
      ScalaAggregationOperator(name, vty, initCode, addCode)
    case ScalaMonoAggregationOperator(name, stateTy, inputTy, outputTy, initCode, addCode, resultCode, combineCode) =>
      ScalaMonoAggregationOperator(name,
        visitType(stateTy),
        visitType(inputTy),
        visitType(outputTy),
        initCode, addCode, resultCode, combineCode
      )
    case _ =>
      super.visitAggregationOperator(op)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case ScalaTerm(code, ty, args, isApp) => Seq(ScalaTerm(code, ty, args.flatMap(visitTerm), isApp))
      case ScalaConstantTerm(code, ty) => Seq(ScalaConstantTerm(code, ty))
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case ScalaType(sty) => ScalaType(sty)
      case _ => super.visitType(ty)
  }