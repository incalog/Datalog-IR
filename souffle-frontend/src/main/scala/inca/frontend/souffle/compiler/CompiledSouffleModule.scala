package inca.frontend.souffle.compiler

import inca.backend.ir.IR
import inca.backend.ir.IR.Name
import inca.compiler.SourceLocation.NoSourceLocation
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.frontend.souffle.Syntax.{Input, PrintSize, RuleSignature}
import inca.runtime.context.DataModel

// TODO generate ir on demand, provide souffle module as input
case class CompiledSouffleModule(ir: IR.Module,
                                 inputs: Seq[(RuleSignature, Input)],
                                 printSizes: Seq[PrintSize],
                                 dataModel: DataModel,
                                 options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
