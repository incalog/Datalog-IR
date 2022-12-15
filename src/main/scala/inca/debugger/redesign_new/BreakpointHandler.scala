package inca.debugger.redesign_new

import inca.backend.analyze.DependencyGraph
import scala.collection.mutable

// We cannot predict what the predicate table will be for example at a specific breakpoint
// Therefore, Breakpoints should not be interested in the predicate table or body table or argument table
// The normalize function is to insert empty tables where possible
case class IRBreakpoint(stopAt: Query, cond: () => Boolean) {
  def normalize: IRBreakpoint = IRBreakpoint(Query.toTableless(stopAt), cond)
}
object IRBreakpoint {
  def apply(stopAt: Query): IRBreakpoint = {
    IRBreakpoint(Query.toTableless(stopAt), () => true)
  }
}

class BreakpointHandler(dependencyGraph: DependencyGraph) {
  private val breakpoints: mutable.Set[IRBreakpoint] = mutable.Set()
  private val evalPointToBreakpoints: mutable.MultiDict[Query, IRBreakpoint] =
    mutable.MultiDict()
  private val predsWithBreakpoint: mutable.MultiSet[String] = mutable.MultiSet()

  def addBreakpoint(bp: IRBreakpoint): Unit = {
    val normalized = bp.normalize
    breakpoints += normalized
    evalPointToBreakpoints += normalized.stopAt -> normalized
    predsWithBreakpoint += normalized.stopAt.predicate
  }

  def removeBreakpoint(bp: IRBreakpoint): Unit = {
    val normalized = bp.normalize
    breakpoints -= normalized
    evalPointToBreakpoints -= normalized.stopAt -> normalized
    predsWithBreakpoint -= normalized.stopAt.predicate
  }

  def clearBreakpoints(): Unit = {
    breakpoints.clear()
    evalPointToBreakpoints.clear()
    predsWithBreakpoint.clear()
  }

  def withBreakpoints[A](bps: Seq[IRBreakpoint])(f: => A): A = {
    val snapBreakpoints = breakpoints
    clearBreakpoints()
    bps.foreach(addBreakpoint)
    try f
    finally {
      clearBreakpoints()
      snapBreakpoints.foreach(addBreakpoint)
    }
  }

  def isAtBreakpoint(q: Query): Boolean = {
    val bps = evalPointToBreakpoints.get(Query.toTableless(q))
    bps.exists(_.cond())
  }

  def breakpointReachableFromCallee(callee: String): Boolean = {
    val transitivelyReachable = dependencyGraph.transitvelyReachable(callee)
    predsWithBreakpoint.exists(transitivelyReachable.contains)
  }

  def breakpointReachableInPredicate(
      query: Query,
      considerCyclic: Boolean
    ): Boolean = {
    var transitivelyReachable = dependencyGraph.transitvelyReachable(query.predicate)
    if (considerCyclic)
      transitivelyReachable += query.predicate
    else
      transitivelyReachable -= query.predicate
    if (predsWithBreakpoint.exists(transitivelyReachable.contains))
      return true

    // TODO
//    val samePredBreakpoints = breakpoints.filter(_.stopAt.predicate == query.pred)
//    samePredBreakpoints.foreach {
//      case IRBreakpoint(QueryResult(_, _), _) =>
//        // query result breakpoint is always reachable
//        return true
//      case _ => // nothing
//    }
//
//    if (samePredBreakpoints.nonEmpty) {
//      query.state match {
//        case QueryState.QueryEntry =>
//          // any breakpoint of this predicate is reachable from predicate entry
//          return true
//        case QueryState.RuleEntry =>
//          // any breakpoint of this predicate is reachable from body entry
//          return true
//        case QueryState.AtAtom =>
//          InRule(_, _, _, current, remainingRules) =>
//          samePredBreakpoints.foreach {
//            case IRBreakpoint(InRule(_, _, _, currentBreak, remainingRulesBreak), _) =>
//              if (remainingRulesBreak.size < remainingRules.size)
//                return true
//              if (
//                remainingRulesBreak.size == remainingRules.size && currentBreak.atoms.size < current.atoms.size
//              )
//                return true
//            case _ => // nothing
//          }
//        case QueryState.QueryEnd =>
//        // nothing
//      }
//    }

    false
  }
}
