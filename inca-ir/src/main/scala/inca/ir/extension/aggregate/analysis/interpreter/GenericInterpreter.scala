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
    val adornment = argBindingInfo.adornment

    // eval the actual call in a new scoped environment
    updateSupplementaryChecked { beforeCall =>
      val relRes = evalRelationEntry(r, params, adornment, evalContext)

      // Make sure we have a well-defined aggregation column
      val aggColInfos = argBindingInfo(aggColumnIndex)
      if ((aggColInfos.size != 1) || aggColInfos.head.isNested)
        failure(InvalidBindings, "Aggregation with partially bound nested values is not supported.")
      val aggColInfo = aggColInfos.head

      // Do not bind anything, just keep everything that was bound before
      // and the aggregate column.
      val filteredInfo = argBindingInfo.update { case (idx, infos) => 
        if (idx == aggColumnIndex)
          Seq(aggColInfo)
        else
          infos.filter(_.isBound)
      }
      val callRes = renameRelationResult(relRes, params, filteredInfo)

      val aggCol = aggColInfo.col
      val expectedAggResult =
        if (aggColInfo.isBound)
          Some(relationOps.project(callRes, Seq(aggCol)))
        else
          None

      // Project everything away that was freshly bound, except for the aggregate column.
      // This is safe, since an aggregation does not bind variables.
      val colsBefore = relationOps.columns(beforeCall) :+ aggCol
      val colsAfter = relationOps.columns(callRes)
      val projected = relationOps.project(callRes, colsAfter.intersect(colsBefore))

      // Perform the aggregation
      val cols = relationOps.columns(projected)
      val colsWithoutAggCol = cols.diff(Seq(aggCol))

      val aggRes = relationOps.groupBy(projected, aggCol, colsWithoutAggCol)(cols, { (groupByValues, accValues) =>
          val aggRes = op match
            case ArithmeticAggregationOperator.Count =>
              aggregateOps.count(r.asInstanceOf[ir.RelationBase], callRes)
            case _ =>
              accValues.foldLeft(aggregateOps.init(op))(aggregateOps.aggregate(_, _, op))
          groupByValues :+ aggRes
      })

      // If the aggregate column was bound, we need to compare the result.
      val filteredAggRes = expectedAggResult match
        case Some(res) => relationOps.naturalJoin(aggRes, res)
        case _ => aggRes
      
      // filteredAggRes still contains the bound columns from before.
      // We natural join to merge the results in.
      relationOps.naturalJoin(beforeCall, filteredAggRes)
    }

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case agg@Aggregate(ref, args, op) =>
      val rel = ref.target match
        case Some(r) => r
        case _ => throw IllegalStateException(s"Unresolved reference to relation ${ref.name}")
      evalAggregate(rel, relationParams(rel), agg.args, agg.aggregationColumns, agg.op)
    case _ => super.evalAtomOpen(at)
