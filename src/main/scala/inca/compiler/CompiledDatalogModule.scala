package inca.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.source.SourceLocation
import inca.compiler.source.SourceLocation.NoSourceLocation
import inca.runtime.context.DataModel

case class CompiledDatalogModule(ir: Datalog.Module, dataModel: DataModel, options: Options)
    extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
