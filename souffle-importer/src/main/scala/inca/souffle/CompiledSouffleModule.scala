package inca.souffle

import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.frontend.parser.SourceLocation
import inca.frontend.parser.SourceLocation.NoSourceLocation
import inca.souffle.Syntax.{Input, PrintSize, RuleSignature}
import inca.{CompiledModule, CompilerOptions}

case class CompiledSouffleModule(ir: GP.Module,
                                 inputs: Seq[(RuleSignature, Input)],
                                 printSizes: Seq[PrintSize],
                                 compilerOptions: CompilerOptions) extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
