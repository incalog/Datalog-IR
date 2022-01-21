package inca.frontend.functional.debugger

import inca.compiler.source.SourceObject
import inca.debugger.{BeforeAfter, ControlPoint}
import inca.frontend.functional.core
import inca.frontend.functional.core.{FunctionDef, If}

sealed trait FunctionalControlPoint

case class FunctionPoint(fun: FunctionDef, point: SourceObject, irPoint: ControlPoint) extends FunctionalControlPoint {
  override def toString: String = s"FunctionPoint(${fun.name}, $point)"

  def vars: Iterable[core.Name] = fun.params.map(_.name) ++ fun.body.vars.keys
}

case class ConditionalPoint(cond: If, thenBranch: Boolean, irPoint: ControlPoint) extends FunctionalControlPoint