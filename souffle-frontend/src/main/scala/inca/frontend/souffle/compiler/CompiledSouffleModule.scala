package inca.frontend.souffle.compiler

import inca.backend.ir.DatalogScala
import inca.backend.ir.DatalogScala.Name
import inca.compiler.source.SourceLocation
import inca.compiler.source.SourceLocation.NoSourceLocation
import inca.compiler.CompiledModule
import inca.frontend.souffle.Syntax.Input
import inca.frontend.souffle.Syntax.PrintSize
import inca.frontend.souffle.Syntax.RuleSignature
import inca.frontend.souffle.Syntax.SouffleModule
import inca.runtime.context.DataModel

// TODO generate ir on demand, provide souffle module as input
case class CompiledSouffleModule(
                                  souffle: SouffleModule,
                                  ir: DatalogScala.Module,
                                  inputs: Map[String, (RuleSignature, Input)],
                                  printSizes: Seq[PrintSize],
                                  dataModel: DataModel,
                                  options: SouffleOptions)
    extends CompiledModule {
  override def name: Name = ir.name

  override def sourceLocation: SourceLocation = NoSourceLocation
}
