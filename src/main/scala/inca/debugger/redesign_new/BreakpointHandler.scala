package inca.debugger.redesign_new

import inca.backend.analyze.DependencyGraph
import scala.collection.mutable

// We cannot predict the value tables of a query at a specific breakpoint
// Therefore, Breakpoints should not be interested in the value table.
// The normalize function inserts empty value table where possible
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
  private val queryToBreakpoint: mutable.MultiDict[Query, IRBreakpoint] = mutable.MultiDict()
  private val breakpointedPreds: mutable.MultiSet[Predicate] = mutable.MultiSet()

  def addBreakpoint(bp: IRBreakpoint): Unit = {
    val normalized = bp.normalize
    breakpoints += normalized
    queryToBreakpoint += normalized.stopAt -> normalized
    breakpointedPreds += normalized.stopAt.pred
  }

  def removeBreakpoint(bp: IRBreakpoint): Unit = {
    val normalized = bp.normalize
    breakpoints -= normalized
    queryToBreakpoint -= normalized.stopAt -> normalized
    breakpointedPreds -= normalized.stopAt.pred
  }

  def clearBreakpoints(): Unit = {
    breakpoints.clear()
    queryToBreakpoint.clear()
    breakpointedPreds.clear()
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

  def isAtBreakpoint(query: Query): Boolean = {
    val bps = queryToBreakpoint.get(Query.toTableless(query))
    bps.exists(_.cond())
  }

  def stepOverReachesBreakpoint(query: Query): Boolean = query match {
    case Subquery(_, _, _, _, Rule(_, _, Atom(atom) +: _) +: _) =>
      atom.asCall match {
        case Some((callee, _)) =>
          val transitivelyReachable = dependencyGraph.transitvelyReachable(callee)
          breakpointedPreds.exists(transitivelyReachable.contains)
        case None => false
      }
    case _ => false
  }

  // need to consider the breakpoints in the current remaing rule and in the following potential iteration
  def stepOutReachesBreakpoint(query: Query, consideredCyclic: Boolean): Boolean = query match {
    case Subquery(pred, _, _, _, rules @ Rule(_, _, atoms @ Atom(atom) +: _) +: _) =>
      val reachablePreds = mutable.Set() ++ dependencyGraph.transitvelyReachable(pred)
      if (consideredCyclic) { // need to consider that predicate will be executed again
        reachablePreds += pred
      } else {
        // only need to consider the following subqueries produces by the remaining atoms in the rule and the remaining rules
        reachablePreds -= pred
      }
      if (breakpointedPreds.exists(reachablePreds.contains)) {
        return true
      }

      val samePredBreakpoints = breakpoints.filter(_.stopAt.pred == pred)
      samePredBreakpoints.foreach {
        case IRBreakpoint(Subquery(_, _, _, _, currentBreak +: remainingBreak), _) =>
          if (remainingBreak.size < rules.tail.size) {
            // means breakpoint is in a rule that has not been processed
            return true
          }

          val currentBreakAtomsSize = currentBreak match {
            case Rule(_, _, atoms) => atoms.size
            case RuleResult(_) => 0
          }
          if (remainingBreak.size == rules.tail.size && currentBreakAtomsSize < atoms.size) {
            // means that breakpoint is in atom of current rule that has not been processed
            return true
          }
        case _ => false
      }
      false
    case _ => false
  }
}
