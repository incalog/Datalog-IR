package inca.frontend.functional.debugger

import inca.compiler.SourceObject
import inca.debugger.{BeforeAfter, ControlPoint}
import inca.frontend.functional.core.FunctionDef

sealed trait ExpressionPoint
//case class

case class FunctionPoint(fun: FunctionDef, point: BeforeAfter[SourceObject], irPoint: ControlPoint)

