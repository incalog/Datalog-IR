package inca.frontend.functional.debugger

import inca.compiler.SourceObject
import inca.debugger.{BeforeAfter, ControlPoint}
import inca.frontend.functional.core.FunctionDef

case class FunctionPoint(fun: FunctionDef, point: BeforeAfter[SourceObject], irPoint: ControlPoint)

