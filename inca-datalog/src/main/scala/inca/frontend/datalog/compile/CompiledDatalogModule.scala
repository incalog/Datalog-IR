package inca.frontend.datalog.compile

import inca.frontend.datalog.syntax.Module
import inca.frontend.datalog.typecheck.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{CompiledModule, Name, Module as IRModule}
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions

case class CompiledDatalogModule(mod: Module, override val compilerOptions: CompilerOptions)
  extends CompiledModule {

  override def name: Name = Name("Datalog")

  override def sourceLocation: SourceLocation = Name("Datalog")

  val datalogLogging = compilerOptions("fun_logging")
  val logTyped: Boolean = datalogLogging.readBoolean("typed")

  lazy val typed: Module = {
    val logMod = datalogLogging.readBoolean("module")

    if (logMod && !logTyped)
      printStep("Module", mod)

    val typer: Typechecker = new Typechecker
    typer.checkModule(mod)

    if (logMod && !logTyped)
      printStep("Module", mod)

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

object CompiledDatalogModule:
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {}
  ) // arith + string + data
