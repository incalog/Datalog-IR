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

  def isAtBreakpoint(evalPoint: EvaluationPoint): Boolean = {
    val breakpoints = evalPointToBreakpoints.get(evalPoint)
    breakpoints.exists(_.cond())
  }

  def breakpointReachable(evalPoint: EvaluationPoint): Boolean = {
    val reachable = dependencyGraph.transitvelyReachable(evalPoint.pred)
    predsWithBreakpoint.exists(reachable.contains)
  }
}
