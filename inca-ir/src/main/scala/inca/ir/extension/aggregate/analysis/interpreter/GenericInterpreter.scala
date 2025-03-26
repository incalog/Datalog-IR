package inca.ir.extension.aggregate.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.effect.NoParamRelation
import inca.ir.{Arg, Atom, ModuleEntry, Name, RefByName, Var}
import inca.ir.analysis.base.interpreter.{Adorn, Adornment, BaseGenericInterpreter, SupColumn}
import inca.ir.extension.aggregate.*
import sturdy.data.MayJoin

trait AggregateOps[V]:
  def init(op: AggregationOperator): V
  def aggregate(accumulator: V, value: V, op: AggregationOperator): V


trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val aggregateOps: AggregateOps[V]

  override def evalArg(arg: ir.Arg)(using Fixed): Option[SupColumn] = arg match
    //case AggregateColumnArg(t) if canDetermineValue(t) => Some(evalTerm(t))
    case AggregateColumnArg(t) => None
    case _ => super.evalArg(arg)

  override def extractVarName(arg: Arg): Option[Name] = arg match
    case AggregateColumnArg(t) => extractVarName(t)
    case _ => super.extractVarName(arg)

  private final def evalAggregate[R <: ModuleEntry](r: R, params: Seq[ir.Param], args: Seq[ir.Arg], aggColumns: Seq[Int], op: AggregationOperator)(using Fixed): Unit =
    if (aggColumns.size != 1)
      throw IllegalStateException("Aggregation is only supported on a single column.")
    val Seq(aggColumnIndex) = aggColumns

    val (evalContext, argMapping) = evaluationContextForCall(r, params, args)
    val adornment = calculateAdornment(argMapping)

    // the expected
    val AggregateColumnArg(t) = args(aggColumnIndex): @unchecked
    val expectedAggResult = if (canDetermineValue(t)) Some(evalTerm(t)) else None

    // TODO: Should this also work with tuple arguments?
    val resultColumn = extractVarName(t).map(_.name).getOrElse(gensym.fresh("agg"))

    // eval the actual call in a new scoped environment
    updateSupplementaryChecked { beforeCall =>
      val relRes = evalRelationLikeEntry(r, params, adornment, evalContext)

      // keep all variables that were bound before the aggregation. Important, do not bind new variables!
      var subst = argMapping.flatMap(beforeAndAfter => beforeAndAfter.map((b, a) => a -> b)).toMap
      // also keep the column we aggregate over
      val aggColumn = relationOps.columns(relRes)(aggColumnIndex)
      subst += aggColumn -> resultColumn

      val validKeys = relationOps.columns(relRes)
      val callRes = relationOps.projectAndRename(relRes, subst.filter(kv => validKeys.contains(kv._1)))
      val callAggColIndex = relationOps.columnIndex(callRes, resultColumn)

      // Perform the aggregation. Note, the rows must already be grouped here! That is, they all look the same
      // except for the column that contains the value to be aggregated.
      val initialValue = aggregateOps.init(op)
      val aggRes = relationOps.fold(callRes, 0.until(subst.size).map(_ => initialValue)) { case (acc, row) =>
        val aggValue = aggregateOps.aggregate(acc(callAggColIndex), row(callAggColIndex), op)
        row.updated(callAggColIndex, aggValue)
      }

      relationOps.naturalJoin(beforeCall, aggRes)
    }

    // assert equalities in case we expected a certain result
    expectedAggResult match
      case Some(expectedResSubColumn) =>
        evalEq(Var(Name(resultColumn)), Var(Name(expectedResSubColumn)), false)
      case None => // nothing

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case agg@Aggregate(ref, args, op) =>
      val rel = ref.target match
        case Some(r) => r
        case _ => throw IllegalStateException(s"Unresolved reference to relation ${ref.name}")
      evalAggregate(rel, relationParams(rel), agg.args, agg.aggregationColumns, agg.op)
    case _ => super.evalAtomOpen(at)
