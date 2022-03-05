package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.SourceLocation.NoSourceLocation
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.frontend.souffle.Syntax.RelationDecl
import inca.runtime.context.DataModel

case class CompiledSouffleModule(ir: Datalog.Module,
                                 inputs: Seq[RelationDecl],
                                 printSizes: Seq[RelationDecl],
                                 dataModel: DataModel,
                                 options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
