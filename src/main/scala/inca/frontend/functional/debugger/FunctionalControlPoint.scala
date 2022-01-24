package inca.frontend.functional.debugger

import inca.compiler.source.SourceObject
import inca.debugger.{BeforeAfter, ControlPoint}
import inca.frontend.functional.core
import inca.frontend.functional.core.{FunctionDef, If}

sealed trait FunctionalControlPoint

case class FunctionPoint(fun: FunctionDef, point: SourceObject, irPoint: ControlPoint) extends FunctionalControlPoint {
  override def toString: String =
    if (isFunctionEntry)
      s"FunctionPoint(enter ${fun.name})"
    else if (isFunctionExit)
      s"FunctionPoint(exit ${fun.name})"
    else
      s"FunctionPoint(${fun.name}, $point)"

  def vars: Iterable[core.Name] = fun.params.map(_.name) ++ fun.body.vars.keys

  def isFunctionEntry: Boolean = fun.name.sourceObject == point
  def isFunctionExit: Boolean = point.loc match {
    case o: FunctionDef => o.name == fun.name
    case _ => false
  }
}

case class ConditionalPoint(cond: If, thenBranch: Boolean, irPoint: ControlPoint) extends FunctionalControlPoint