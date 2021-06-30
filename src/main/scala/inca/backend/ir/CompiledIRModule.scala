package inca.backend.ir

import inca.backend.ir.IR.Name
import inca.compiler.SourceLocation.NoSourceLocation
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.runtime.context.DataModel

case class CompiledIRModule(ir: IR.Module, dataModel: DataModel, options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
