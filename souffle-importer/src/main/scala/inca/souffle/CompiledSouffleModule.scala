package inca.souffle

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.compiler.{CompiledModule, Options}
import inca.frontend_old.parser.SourceLocation
import inca.frontend_old.parser.SourceLocation.NoSourceLocation
import inca.souffle.Syntax.{Input, PrintSize, RuleSignature}

// TODO generate ir on demand, provide souffle module as input
case class CompiledSouffleModule(ir: GP.Module,
                                 inputs: Seq[(RuleSignature, Input)],
                                 printSizes: Seq[PrintSize],
                                 options: Options) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
