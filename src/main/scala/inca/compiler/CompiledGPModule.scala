package inca.compiler

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.frontend.parser.SourceLocation
import inca.frontend.parser.SourceLocation.NoSourceLocation

case class CompiledGPModule(ir: GP.Module, options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
