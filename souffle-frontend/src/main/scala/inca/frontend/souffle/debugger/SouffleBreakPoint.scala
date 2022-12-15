package inca.frontend.souffle.debugger

import inca.debugger.redesign_old.IRBreakpoint
import inca.frontend.souffle.Syntax.Statement

sealed trait SouffleBreakPoint
case class PatternEndBreakPoint(pred: String) extends SouffleBreakPoint
case class InputBreakPoint(pred: String, before: Boolean) extends SouffleBreakPoint
case class InRuleBreakPoint(pred: String, ruleIdx: Int, stm: Option[Statement])
    extends SouffleBreakPoint
