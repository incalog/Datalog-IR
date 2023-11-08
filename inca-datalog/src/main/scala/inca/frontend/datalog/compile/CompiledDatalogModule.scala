package inca.frontend.datalog.compile

import inca.frontend.datalog.syntax.Module
import inca.frontend.datalog.typecheck.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{CompiledModule, Name, Module as IRModule}
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.ir.extension.{aggregateset, set, bool, datamatch, block, disjunction, not, impure, demand, tuple}

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
