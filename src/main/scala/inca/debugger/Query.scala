package inca.debugger

import inca.backend.ir.Datalog
import inca.debugger.ValueTable

sealed trait QueryState
object QueryState {
  case object RuleResult extends QueryState
  case object RuleMerge extends QueryState
  case object NextAtom extends QueryState
  case object QueryUnion extends QueryState
  case object QueryEnd extends QueryState
  case object QueryResult extends QueryState
}

sealed trait TableSign
case object PositiveTable extends TableSign
case object NegativeTable extends TableSign

sealed trait Query {
  def pred: Predicate
  def state: QueryState
}
object Query {
  def toTableless(q: Query): Query = q match {
    case Subquery(p, _, _, _, bodies) =>
      val tablelessRules = bodies.map {
        case RuleResult(_) => RuleResult(ValueTable.empty(Seq()))
        case b => b
      }
      Subquery(
        p,
        ValueTable.empty(Seq()),
        ValueTable.empty(Seq()),
        ValueTable.empty(Seq()),
        tablelessRules)
    case QueryResult(p, _, _) => QueryResult(p, ValueTable.empty(Seq()), ValueTable.empty(Seq()))
  }
}
case class Subquery(
    pred: Predicate,
    args: ValueTable,
    result: ValueTable,
    supplementary: ValueTable,
    bodies: Seq[RuleEval])
    extends Query {
  override def state: QueryState =
    if (bodies.isEmpty)
      QueryState.QueryEnd
    else
      bodies.head match {
        case Rule(_, _, atoms) =>
          if (atoms.isEmpty)
            QueryState.RuleResult
          else
            atoms.head match {
              case Atom(_) => QueryState.NextAtom
              case AtomResult(_, _) => QueryState.RuleMerge
            }
        case RuleResult(_) => QueryState.QueryUnion
      }
}

case class QueryResult(pred: Predicate, args: ValueTable, result: ValueTable) extends Query {
  override val state: QueryState = QueryState.QueryResult
}

sealed trait RuleEval
case class Rule(pred: Predicate, params: Seq[Datalog.Name], atoms: Seq[AtomEval]) extends RuleEval
case class RuleResult(t: ValueTable) extends RuleEval

sealed trait AtomEval
case class Atom(a: Datalog.Atom) extends AtomEval
case class AtomResult(t: ValueTable, sign: TableSign = PositiveTable) extends AtomEval
