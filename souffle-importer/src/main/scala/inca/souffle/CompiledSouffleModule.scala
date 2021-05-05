package inca.souffle

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.compiler.SourceLocation.NoSourceLocation
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.runtime.context.DataModel
import inca.souffle.Syntax.{Input, PrintSize, RuleSignature}

// TODO generate ir on demand, provide souffle module as input
case class CompiledSouffleModule(ir: GP.Module,
                                 inputs: Seq[(RuleSignature, Input)],
                                 printSizes: Seq[PrintSize],
                                 dataModel: DataModel,
                                 options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation

}
