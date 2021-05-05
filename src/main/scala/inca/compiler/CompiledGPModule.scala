package inca.compiler

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.compiler.SourceLocation.NoSourceLocation
import inca.runtime.context.DataModel

case class CompiledGPModule(ir: GP.Module, options: Options) extends CompiledModule {
  override def name: Name = ir.name
  override def sourceLocation: SourceLocation = NoSourceLocation
  override def dataModel: DataModel = options.languageMetaInfo
}
