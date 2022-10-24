package inca.debugger.redesign

import inca.backend.analyze.DependencyGraph
import scala.collection.mutable

// We cannot predict what the predicate table will be for example at a specific breakpoint
// Therefore, Breakpoints should not be interested in the predicate table or body table or argument table
// The normalize function is to insert empty tables where possible
case class IRBreakpoint(stopAt: EvaluationPoint, cond: () => Boolean) {
  def normalize: IRBreakpoint = IRBreakpoint(EvaluationPoint.toTableless(stopAt), cond)
}
object IRBreakpoint {
  def apply(stopAt: EvaluationPoint): IRBreakpoint = {
    IRBreakpoint(EvaluationPoint.toTableless(stopAt), () => true)
  }
}

class BreakpointHandler(dependencyGraph: DependencyGraph) {
  private val breakpoints: mutable.Set[IRBreakpoint] = mutable.Set()
  private val evalPointToBreakpoints: mutable.MultiDict[EvaluationPoint, IRBreakpoint] =
    mutable.MultiDict()
  private val predsWithBreakpoint: mutable.MultiSet[String] = mutable.MultiSet()

  def addBreakpoint(bp: IRBreakpoint): Unit = {
    val normalized = bp.normalize
    breakpoints += normalized
    evalPointToBreakpoints += normalized.stopAt -> normalized
    predsWithBreakpoint += normalized.stopAt.pred
  }

  def removeBreakpoint(bp: IRBreakpoint): Unit = {
    val normalized = bp.normalize
    breakpoints -= normalized
    evalPointToBreakpoints -= normalized.stopAt -> normalized
    predsWithBreakpoint -= normalized.stopAt.pred
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
    try f finally {
      clearBreakpoints()
      snapBreakpoints.foreach(addBreakpoint)
    }
  }

  def isAtBreakpoint(evalPoint: EvaluationPoint): Boolean = {
    val bps = evalPointToBreakpoints.get(EvaluationPoint.toTableless(evalPoint))
    bps.exists(_.cond())
  }

  def breakpointReachableFromCallee(callee: String): Boolean = {
    val transitivelyReachable = dependencyGraph.transitvelyReachable(callee)
    predsWithBreakpoint.exists(transitivelyReachable.contains)
  }

  def breakpointReachableInPredicate(evalPoint: EvaluationPoint, considerCyclic: Boolean): Boolean = {
    var transitivelyReachable = dependencyGraph.transitvelyReachable(evalPoint.pred)
    if (considerCyclic)
      transitivelyReachable += evalPoint.pred
    else
      transitivelyReachable -= evalPoint.pred
    if (predsWithBreakpoint.exists(transitivelyReachable.contains))
      return true

    val samePredBreakpoints = breakpoints.filter(_.stopAt.pred == evalPoint.pred)
    samePredBreakpoints.foreach {
      case IRBreakpoint(EvaluationResult(_, _), _) =>
        // evaluation result breakpoint is always reachable
        return true
      case _ => // nothing
    }

    if (samePredBreakpoints.nonEmpty) {
      evalPoint match {
        case _: PredicateEntry =>
          // any breakpoint of this predicate is reachable from predicate entry
          return true
        case _: BeforeRule =>
          // any breakpoint of this predicate is reachable from predicate entry
          return true
        case InRule(_, _, _, current, remainingRules) =>
          samePredBreakpoints.foreach {
            case IRBreakpoint(InRule(_, _, _, currentBreak, remainingRulesBreak), _) =>
              if (remainingRulesBreak.size < remainingRules.size)
                return true
              if (remainingRulesBreak.size == remainingRules.size && currentBreak.atoms.size < current.atoms.size)
                return true
            case _ => // nothing
          }
        case _: EvaluationResult =>
          // nothing
      }
    }

    false
  }
}
