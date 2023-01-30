package inca.frontend.functional.debugger

import inca.compiler.source.SourceObject
import inca.debugger.Query
import inca.frontend.functional.core
import inca.frontend.functional.core.FunctionDef
import inca.frontend.functional.core.If
import inca.frontend.functional.core.Match
import inca.frontend.functional.core.Pattern

sealed trait FunctionalControlPoint {
  val fun: FunctionDef
  val point: SourceObject
  val irQuery: Query

  def vars: Iterable[core.Name] = fun.params.map(_.name) ++ fun.body.vars.keys

  def isFunctionEntry: Boolean = fun.name.sourceObject == point
  def isFunctionExit: Boolean = point.loc match {
    case o: FunctionDef => o.name == fun.name
    case _ => false
  }
}

case class FunctionPoint(fun: FunctionDef, point: SourceObject, irQuery: Query)
    extends FunctionalControlPoint {
  override def toString: String =
    if (isFunctionEntry)
      s"FunctionPoint(enter ${fun.name})"
    else if (isFunctionExit)
      s"FunctionPoint(exit ${fun.name})"
    else
      s"FunctionPoint(${fun.name}, $point)"
}

case class ConditionPoint(fun: FunctionDef, cond: If, thenBranch: Boolean, irQuery: Query)
    extends FunctionalControlPoint {
  override val point: SourceObject = cond.sourceObject
  override def toString: String =
    s"ConditionPoint(${fun.name}, ${cond.cnd.sourceObject}, $thenBranch)"
}
case class MatchPoint(fun: FunctionDef, ma: Match, pat: Pattern, irQuery: Query)
    extends FunctionalControlPoint {
  override val point: SourceObject = pat.sourceObject
  override def toString: String =
    s"MatchPoint(${fun.name}, ${ma.matchee.sourceObject}, ${pat.sourceObject})"
}
