package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.effect.{BaseIRFailure, InvalidBindings, NoParamRelation}
import inca.ir.{Arg, Atom, ModuleEntry, Name, RefByName, Var}
import inca.ir.analysis.base.interpreter.{Adorn, Adornment, BaseGenericInterpreter, BindingInfo, IndexPath, SupColumn}
import inca.ir.extension.aggregate.*
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator
import sturdy.data.MayJoin

case object UnknownAggregationOperator extends BaseIRFailure

trait AggregateOps[V, RV]:
  def init(op: AggregationOperator): V
  def aggregate(accumulator: V, value: V, op: AggregationOperator): V
  def count(rel: ir.RelationBase, rv: RV): V


trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  lazy val aggregateOps: AggregateOps[V, RV]

  override protected def extractBindingInfo(arg: ir.Arg)(using rec: Fixed): Seq[BindingInfo] = arg match
    case AggregateColumnArg(t) => extractBindingInfo(t)
    case _ => super.extractBindingInfo(arg)

  private final def evalAggregate[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], aggColumns: Seq[Int], op: AggregationOperator)(using Fixed): Unit =
    if (aggColumns.size != 1)
      throw IllegalStateException("Aggregation is only supported on a single column.")
    val Seq(aggColumnIndex) = aggColumns

    val (evalContext, argBindingInfo) = evaluationContextForCall(r, params, args)
    val adornment = calculateAdornment(argBindingInfo)

    // eval the actual call in a new scoped environment
    updateSupplementaryChecked { beforeCall =>
      val relRes = evalRelationEntry(r, params, adornment, evalContext)

      // Make sure we have a well-defined aggregation column
      val aggColInfos = argBindingInfo(aggColumnIndex)
      if ((aggColInfos.size != 1) || !aggColInfos.head.isToplevel)
        failure(InvalidBindings, "Aggregation with partially bound nested values is not supported.")
      val aggColInfo = aggColInfos.head

      // Do not bind anything, just keep everything that was bound before
      // and the aggregate column.
      val filteredInfo = argBindingInfo.zipWithIndex.map {
        case (infos, idx) if idx != aggColumnIndex => infos.filter(_.isBound)
        case _ => Seq()
      }
      val combinedInfo = filteredInfo.updated(aggColumnIndex, Seq(aggColInfo))
      val callRes = renameRelationResult(relRes, params, combinedInfo)
      val callAggColIndex = relationOps.columnIndex(callRes, aggColInfo.col)

      val expectedAggResult =
        if (aggColInfo.isBound)
          Some(relationOps.project(callRes, Seq(aggColInfo.col)))
        else
          None

      // Perform the aggregation
      val cols = relationOps.columns(callRes)

      val aggRes = op match
        case ArithmeticAggregationOperator.Count =>
          // special case for count aggregation
          val colsWithAggColDropped = cols.patch(aggColumnIndex, Nil, 1)
          val aggRes = aggregateOps.count(r.asInstanceOf[ir.RelationBase], callRes)
          relationOps.fold(callRes, Seq()) { case (acc, row) =>
            row.updated(callAggColIndex, aggRes)
          }
        case _ =>
          val initialRow = cols.indices.map(_ => aggregateOps.init(op))
          relationOps.fold(callRes, initialRow) { case (acc, row) =>
            val aggValue = aggregateOps.aggregate(acc(callAggColIndex), row(callAggColIndex), op)
            row.updated(callAggColIndex, aggValue)
          }

      val filteredAggRes = expectedAggResult match
        case Some(res) => relationOps.naturalJoin(aggRes, res)
        case _ => aggRes

      relationOps.naturalJoin(beforeCall, filteredAggRes)
    }

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case agg@Aggregate(ref, args, op) =>
      val rel = ref.target match
        case Some(r) => r
        case _ => throw IllegalStateException(s"Unresolved reference to relation ${ref.name}")
      evalAggregate(rel, relationParams(rel), agg.args, agg.aggregationColumns, agg.op)
    case _ => super.evalAtomOpen(at)
