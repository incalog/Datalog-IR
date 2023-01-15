package inca.debugger.redesign_new

import inca.backend.ir.Datalog
import inca.debugger.table.ImmutableTable

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
      val tablelessBodies = bodies.map {
        case RuleResult(_) => RuleResult(ImmutableTable.empty(Seq()))
        case b => b
      }
      Subquery(
        p,
        ImmutableTable.empty(Seq()),
        ImmutableTable.empty(Seq()),
        ImmutableTable.empty(Seq()),
        tablelessBodies)
    case QueryResult(p, _) => QueryResult(p, ImmutableTable.empty(Seq()))
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

case class QueryResult(pred: Predicate, t: ValueTable) extends Query {
  override val state: QueryState = QueryState.QueryResult
}

sealed trait RuleEval
case class Rule(pred: Predicate, params: Seq[Datalog.Name], atoms: Seq[AtomEval]) extends RuleEval
case class RuleResult(t: ValueTable) extends RuleEval

sealed trait AtomEval
case class Atom(a: Datalog.Atom) extends AtomEval
case class AtomResult(t: ValueTable, sign: TableSign = PositiveTable) extends AtomEval
