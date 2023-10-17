package inca.frontend.datalog.compile

import inca.frontend.datalog.syntax.Module
import inca.frontend.datalog.typecheck.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{CompiledModule, Name, Module as IRModule}
import inca.viatra.compile.{GeneratePSystem, PSystem}

case class CompiledDatalogModule(mod: Module) extends CompiledModule {

  override def name: Name = Name("Datalog")

  override def sourceLocation: SourceLocation = Name("Datalog")

  lazy val typed: Module = {
    val typer: Typechecker = new Typechecker
    typer.checkModule(mod)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    mod
  }

  lazy val ir: IRModule = {
    val compiler = new GenerateIR
    val module = compiler.compileModule(typed)
    module
  }
}
