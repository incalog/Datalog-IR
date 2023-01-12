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
  def predicate: Datalog.Name
  def state: QueryState
}
object Query {
  def toTableless(q: Query): Query = q match {
    case Subquery(p, _, _, _, bodies) =>
      val tablelessBodies = bodies.map {
        case RuleResult(_, s) => RuleResult(ImmutableTable.empty(Seq()), s)
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
    predicate: Datalog.Name,
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
              // val Subquery(p, args, result, sup, Rule(_, params, atoms) :: rulesTail) = q
              case AtomResult(_, _) => QueryState.RuleMerge
            }
        case RuleResult(_, _) => QueryState.QueryUnion
      }
}

case class QueryResult(predicate: Datalog.Name, t: ValueTable) extends Query {
  override val state: QueryState = QueryState.QueryResult
}

sealed trait RuleEval
case class Rule(predicate: Datalog.Name, params: Seq[Datalog.Param], atoms: Seq[AtomEval])
    extends RuleEval
case class RuleResult(t: ValueTable, sign: TableSign = PositiveTable) extends RuleEval

sealed trait AtomEval
case class Atom(a: Datalog.Atom) extends AtomEval
case class AtomResult(t: ValueTable, sign: TableSign = PositiveTable) extends AtomEval
