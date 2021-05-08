package inca.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.SourceLocation.NoSourceLocation
import inca.runtime.context.DataModel

case class CompiledGPModule(ir: Datalog.Module, dataModel: DataModel, options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
