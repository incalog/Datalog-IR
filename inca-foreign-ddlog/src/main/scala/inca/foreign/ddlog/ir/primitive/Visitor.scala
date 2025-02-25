package inca.foreign.ddlog.ir.primitive

import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.Aggregate
import inca.ir.extension.mono.MonoAggregationOperator
import inca.ir.visitors.BaseIRVisitor
import inca.ir.*

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor with aggregate.Visitor:
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case DDLogDefnModuleEntry(name, defn) => Seq(DDLogDefnModuleEntry(name, defn))
    case _ => super.visitModuleEntry(moduleEntry)

  override def visitAggregationOperator(op: aggregate.AggregationOperator): aggregate.AggregationOperator = op match
    case DDLogAggregationOperator(name, ty, initCode, addCode) =>
      val vty = visitType(ty)
      DDLogAggregationOperator(name, vty, initCode, addCode)
    case _ =>
      super.visitAggregationOperator(op)