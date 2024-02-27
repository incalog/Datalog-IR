package inca.frontend.datalog.compile

import inca.frontend.datalog.syntax.Module
import inca.frontend.datalog.typecheck.Typechecker
import inca.ir.extension.*
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{CompiledModule, Name, Module as IRModule}

case class CompiledDatalogModule(mod: Module, override val compilerOptions: DatalogCompilerOptions) extends CompiledModule {

  override def name: Name = Name("Datalog")

  override def sourceLocation: SourceLocation = Name("Datalog")

  val logTyped: Boolean = compilerOptions.datalogLogging.logTypeInformation

  lazy val typed: Module = {
    val logMod = compilerOptions.datalogLogging.logModule

    if (logMod && !logTyped)
      printStep("Datalog-Module", mod)

    val typer: Typechecker = new Typechecker
    typer.checkModule(mod)

    if (logMod && logTyped)
      printStep("Datalog-Module", mod)

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
