package inca.debugger

case class BreakpointIR(cp: ControlPoint, cond: () => Boolean)
object BreakpointIR {
  def apply(cp: ControlPoint): BreakpointIR = {
    BreakpointIR(cp, () => true)
  }
}
