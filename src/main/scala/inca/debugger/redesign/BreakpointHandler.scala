package inca.debugger.redesign

import inca.backend.analyze.DependencyGraph
import scala.collection.mutable

// TODO this breakpoint design is not well-suited because it contains pred table and result table
// we cannot predict these
// we either need a different form of equality for eval points or we need to use something different
case class IRBreakpoint(stopAt: EvaluationPoint, cond: () => Boolean)
object IRBreakpoint {
  def apply(stopAt: EvaluationPoint): IRBreakpoint = {
    IRBreakpoint(stopAt, () => true)
  }
}

class BreakpointHandler(dependencyGraph: DependencyGraph) {
  private val breakpoints: mutable.Set[IRBreakpoint] = mutable.Set()
  private val evalPointToBreakpoints: mutable.MultiDict[EvaluationPoint, IRBreakpoint] =
    mutable.MultiDict()
  private val predsWithBreakpoint: mutable.MultiSet[String] = mutable.MultiSet()

  def addBreakpoint(bp: IRBreakpoint): Unit = {
    breakpoints += bp
    evalPointToBreakpoints += bp.stopAt -> bp
    predsWithBreakpoint += bp.stopAt.pred
  }

  def removeBreakpoint(bp: IRBreakpoint): Unit = {
    breakpoints -= bp
    evalPointToBreakpoints -= bp.stopAt -> bp
    predsWithBreakpoint -= bp.stopAt.pred
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
