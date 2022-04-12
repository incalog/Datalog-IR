package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.SourceLocation.NoSourceLocation
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.frontend.souffle.Syntax.{Directive, RelationDecl}
import inca.runtime.context.DataModel

// TODO: inputs and outputs with info about delimiters etc.

case class CompiledSouffleModule(ir: Datalog.Module,
                                 inputs: Map[String, (RelationDecl, Directive)],
                                 outputs: Map[String, (RelationDecl, Directive)],
                                 printSizes: Seq[RelationDecl],
                                 dataModel: DataModel,
                                 options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
