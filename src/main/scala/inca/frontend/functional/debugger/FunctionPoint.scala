package inca.frontend.functional.debugger

import inca.compiler.SourceObject
import inca.debugger.{BeforeAfter, ControlPoint}
import inca.frontend.functional.core
import inca.frontend.functional.core.FunctionDef

case class FunctionPoint(fun: FunctionDef, point: SourceObject, irPoint: ControlPoint) {
  override def toString: String = s"FunctionPoint(${fun.name}, $point)"

  def vars: Iterable[core.Name] = fun.params.map(_.name) ++ fun.body.vars.keys
}

