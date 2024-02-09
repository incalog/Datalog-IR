package inca.souffle.frontend.compile

import inca.ir
import inca.ir.extension.{block, bool, datamatch, disjunction, not, set}
import inca.ir.{CompiledModule, Module, Name}
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.souffle.syntax.{Parser, Program}
import inca.util.compileroptions.CompilerOptions

import scala.io.Source

case class CompiledSouffleModule(name: Name, program: Program, compilerOptions: CompilerOptions = CompilerOptions.default) extends CompiledModule {
  override def sourceLocation: SourceLocation = program

  setPipeline(
    List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {}
    )
  )
  
  override def ir: Module =
    val genIR = new GenerateIR
    genIR.compileProgram(program, name.name)
}

object CompiledSouffleModule:
  def fromSource(name: Name, source: Source, compilerOptions: CompilerOptions = CompilerOptions.default): CompiledSouffleModule =
    val content = source.getLines().mkString("\n")
    source.close()
    fromSourceCode(name, content, compilerOptions)

  def fromSourceCode(name: Name, source: String, compilerOptions: CompilerOptions = CompilerOptions.default): CompiledSouffleModule =
    val program: Program = Parser.parseSouffle(source)
    new CompiledSouffleModule(name, program, compilerOptions)