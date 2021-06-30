package inca.frontend.functional.compiler

import inca.backend.ir.IR
import inca.backend.ir.IR.Name
import inca.compiler.{CompiledModule, CompilerFlags, SourceLocation}
import inca.frontend.functional.core.Module
import inca.frontend.functional.lowering.{Defunctionalize, GenerateDataModel, GenerateIR}
import inca.frontend.functional.parser.Parser
import inca.frontend.functional.typechecker.Typechecker
import inca.runtime.context.DataModel

object CompiledFunctionalModule {
  def apply(source: String, options: FunctionalOptions): CompiledFunctionalModule =
    CompiledFunctionalModule(Parser.parse(source), options)
}
case class CompiledFunctionalModule(fun: Module, options: FunctionalOptions) extends CompiledModule {

  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typer = new Typechecker {}

  lazy val typed: Module = {
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val coreModule: Module = {
    val module = new Defunctionalize(typed).transModule()
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Core Module")
      println(module)
    }
    module
  }

  lazy val ir: IR.Module = {
    val module = new GenerateIR(coreModule).transModule()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Intermediate Representation")
      println(module)
    }
    module
  }

  lazy val dataModel: DataModel = {
    val res = new GenerateDataModel(coreModule).transModule()
    res
  }
}
