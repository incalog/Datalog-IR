package inca.frontend.functional.debugger

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.ir.Datalog
import inca.debugger.{ControlPoint, DebuggerFrontend}
import inca.frontend.functional.core.{FunctionDef, Module}

class FunctionalDebuggerFrontend extends DebuggerFrontend {
  override type FrontendPoint = FunctionPoint

  var module: Module = _

  override def initialize(mod: Datalog.Module): Unit = mod.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(m: Module)) => this.module = m
    case h => throw new IllegalArgumentException(s"Cannot create functional debugger frontend for ${mod.name}: No functional module source construct found $h")
  }

  def getFunction(pat: Datalog.Pattern): Option[FunctionDef] = pat.getHint(SourceConstruct.key) match {
    case Some(SourceConstruct(f: FunctionDef)) => Some(f)
    case _ => None
  }

  override def frontendPoint(cp: ControlPoint): Option[FunctionPoint] = {
    val fun = getFunction(cp.point.pat).getOrElse(return None)
    // TODO implement
    ???
  }
}
