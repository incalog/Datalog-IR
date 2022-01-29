package inca.frontend.souffle.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.source.SourceLocation
import inca.compiler.source.SourceLocation.NoSourceLocation
import inca.compiler.{CompiledModule, Options}
import inca.frontend.souffle.Syntax.{Input, PrintSize, RuleSignature, SouffleModule}
import inca.runtime.context.DataModel

// TODO generate ir on demand, provide souffle module as input
case class CompiledSouffleModule(souffle: SouffleModule,
                                 ir: Datalog.Module,
                                 inputs: Map[String, (RuleSignature, Input)],
                                 printSizes: Seq[PrintSize],
                                 dataModel: DataModel,
                                 options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
