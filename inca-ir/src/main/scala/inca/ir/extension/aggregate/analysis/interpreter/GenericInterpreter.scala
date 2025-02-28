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

  private final def evalAggregate(aggregate: Aggregate)(using Fixed): Unit =
    val rel = aggregate.rel.target match
      case Some(r) => r
      case _ => throw IllegalStateException(s"Unresolved reference to relation ${aggregate.rel}")
    val params = rel.params
    val args = aggregate.args
    val aggColumns = aggregate.aggregationColumns
    if (aggregate.aggregationColumns.size != 1)
      throw IllegalStateException("Aggregation is only supported on a single column.")
    val Seq(aggColumnIndex) = aggregate.aggregationColumns

    if (params.isEmpty)
      failure(NoParamRelation, s"Relation ${rel.name} has no Parameters!")

    val argMapping = params.zip(args).map { (p, a) => evalArg(a).map(_ -> p.name.name) }
    val multiMapping = argMapping.flatten.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

    val evalContext = relationOps.projectAndRenameWithMultipleAliases(supplementaryTable.getTable, multiMapping)

    val adornment = Adornment(argMapping.map {
      case Some(_) => Adorn.b
      case None => Adorn.f
    })

    // the expected
    val AggregateColumnArg(t) = args(aggColumnIndex): @unchecked
    val expectedAggResult = if (canDetermineValue(t)) Some(evalTerm(t)) else None
    val resultColumn = extractVarName(t).map(_.name).getOrElse(gensym.fresh("agg"))

    // eval the actual call in a new scoped environment
    updateSupplementaryChecked { beforeCall =>
      supplementaryTable.setTable(evalContext)
      val relRes =
        if (interRelational)
          evalRelation(rel, adornment)
        else
          // assume top for all unbound arguments
          val unboundArgIndices = argMapping.zipWithIndex.filter(_._1.isEmpty).map(_._2)
          unboundArgIndices.map(params).foldLeft[RV](evalContext) {
            case (acc, param) => relationOps.map(acc, param.name.name)(_ => topV)
          }

      // Keep all variables that were bound before the aggregation. Important, do not bind new variables!
      var subst = argMapping.flatMap(beforeAndAfter => beforeAndAfter.map((b, a) => a -> b)).toMap
      // Also keep the column we aggregate over
      val aggColumn = relationOps.columns(relRes)(aggColumnIndex)
      subst += aggColumn -> resultColumn

      val validKeys = relationOps.columns(relRes)
      val callRes = relationOps.projectAndRename(relRes, subst.filter(kv => validKeys.contains(kv._1)))
      val callAggColIndex = relationOps.columnIndex(callRes, resultColumn)

      // Perform the aggregation. Note, the rows must already be grouped here! That is, they all look the same
      // except for the column that contains the value to be aggregated.
      val op = aggregate.op
      val initialValue = aggregateOps.init(op)
      val aggRes = relationOps.fold(callRes, 0.until(subst.size).map(_ => initialValue)) { case (acc, row) =>
        val aggValue = aggregateOps.aggregate(acc(callAggColIndex), row(callAggColIndex), op)
        row.updated(callAggColIndex, aggValue)
      }

      val afterCall = relationOps.naturalJoin(beforeCall, aggRes)
      afterCall
    }

    // assert equalities in case we expected a certain result
    expectedAggResult match
      case Some(expectedResSubColumn) =>
        evalEq(Var(Name(resultColumn)), Var(Name(expectedResSubColumn)), false)
      case None => // nothing

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case agg@Aggregate(ref, args, op) => evalAggregate(agg)
    case _ => super.evalAtomOpen(at)
