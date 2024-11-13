package inca.souffle.frontend.compile

import inca.ir.util.SourceLocation
import inca.ir.{CompiledUnit, Name, Module}
import inca.util.compileroptions.CompilerOptions

case class CompiledSouffleUnit(
                                name: Name, irModules: Seq[Module],
                                otherUnits: Seq[CompiledUnit],
                                isClosedWorld: Boolean,
                                compilerOptions: CompilerOptions
                              ) extends CompiledUnit:
  override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
