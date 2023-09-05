package inca.frontend.constraint.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.source.SourceLocation
import inca.compiler.CompiledModule
import inca.compiler.CompilerFlags
import inca.frontend.constraint.core
import inca.frontend.constraint.datamodelresolver.DataModelResolver
import inca.frontend.constraint.datamodelresolver.DirectDataModelResolver
import inca.frontend.constraint.datamodelresolver.NativeDataModelResolver
import inca.frontend.constraint.desugar.Desugar
import inca.frontend.constraint.extensions
import inca.frontend.constraint.lowering.GenerateDatalog
import inca.frontend.constraint.typechecker.CoreTypechecker
import inca.runtime.context.DataModel

case class CompiledConstraintModule(module: core.Module, options: ConstraintOptions)
    extends CompiledModule {

  override def name: Name = module.name.name

  override def sourceLocation: SourceLocation = module.name

  lazy val dataModel: DataModel = {
    dataModelResolver.resolve(module)
  }
  // FIXME: hack such that dataModel in typer is not recursively defined
  private lazy val _dataModel: DataModel = dataModel

  private lazy val dataModelResolver: DataModelResolver =
    new DataModelResolver with NativeDataModelResolver with DirectDataModelResolver {}

  private lazy val typer: CoreTypechecker = new CoreTypechecker
    with extensions.boolOps.Typechecker
    with extensions.evalCall.Typechecker
    with extensions.forallExists.Typechecker
    with extensions.foreach.Typechecker
    with extensions.ifThenElse.Typechecker
    with extensions.match_.Typechecker
    with extensions.switch_.Typechecker {
    override val dataModel: DataModel = _dataModel
  }

  lazy val typed: core.Module = {
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val desugared: core.Module = {
    val module = Desugar(options.desugarables)(typed)
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    if (CompilerFlags.DEBUGMODE) {
      println(s"Core Module")
      println(module)
    }
    stopIfNeeded()
    module
  }

  lazy val ir: Datalog.Module = {
    val module = new GenerateDatalog().transformModule(desugared)
    if (CompilerFlags.DEBUGMODE) {
      println(s"Intermediate Representation")
      println(module)
    }
    module
  }
}
