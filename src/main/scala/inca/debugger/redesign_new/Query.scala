package inca.debugger.redesign_new

import inca.backend.ir.Datalog
import inca.debugger.table.ImmutableTable
import inca.debugger.Value

sealed trait QueryState
object QueryState {
  case object QueryEntry extends QueryState
  case object RuleEntry extends QueryState
  case object AtAtom extends QueryState
  case object RuleEnd extends QueryState
  case object QueryEnd extends QueryState
  case object QueryResult extends QueryState

//  case Subquery(_, _, _, _, Rule(_, _, Nil) :: _) =>
//  ruleResult(top)
//  case Subquery(_, _, _, _, Rule(_, _, AtomResult(_, _) :: _) :: _) =>
//  ruleMerge(top)
//  case Subquery(_, _, _, _, Rule(_, _, Atom(_) :: _) :: _) =>
//  nextAtom(top)
//  case Subquery(_, _, _, _, RuleResult(_, _) :: _) =>
//  queryUnion(top)
//  case Subquery(_, _, _, _, Nil) =>
//  queryEnd(top)
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
    case QueryResult(p, _, s) => QueryResult(p, ImmutableTable.empty(Seq()), s)
  }
}
case class Subquery(
    predicate: Datalog.Name,
    args: ImmutableTable[Value],
    result: ImmutableTable[Value],
    supplementary: ImmutableTable[Value],
    bodies: Seq[RuleEval])
    extends Query {
  override def state: QueryState =
    if (bodies.isEmpty) QueryState.QueryEnd
    else bodies.head.queryState
}

case class QueryResult(
    predicate: Datalog.Name,
    t: ImmutableTable[Value],
    sign: TableSign = PositiveTable)
    extends Query {
  override val state: QueryState = QueryState.QueryResult
}

sealed trait RuleEval {
  def queryState: QueryState
}
case class Rule(predicate: Datalog.Name, params: Seq[Datalog.Param], atoms: Seq[AtomEval])
    extends RuleEval {
  override def queryState: QueryState =
    if (atoms.isEmpty) QueryState.RuleEnd
    else QueryState.AtAtom
}
case class RuleResult(t: ImmutableTable[Value], sign: TableSign = PositiveTable) extends RuleEval {
  override val queryState: QueryState = QueryState.RuleEnd
}
sealed trait AtomEval
case class Atom(a: Datalog.Atom) extends AtomEval
case class AtomResult(t: ImmutableTable[Value], sign: TableSign = PositiveTable) extends AtomEval
