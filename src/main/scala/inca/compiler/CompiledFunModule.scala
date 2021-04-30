package inca.compiler

import fastparse.Parsed
import inca.backend.ir.GP
import inca.backend.ir.GP.Name
import inca.frontend.core.CompileToGP
import inca.frontend.core.tree._
import inca.frontend.desugar.Desugar
import inca.frontend.parser.{CoreParser, SourceLocation}
import inca.frontend.extensions
import inca.frontend.datamodelresolver.{DirectDataModelResolver, DataModelResolver, NativeDataModelResolver}
import inca.frontend.typechecker.CoreTypechecker
import inca.runtime.context.DataModel

case class CompiledFunModule(module: Module, options: Options) extends CompiledModule {

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

  lazy val typed: Module = {
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val desugared: Module = {
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

  lazy val ir: GP.Module = {
    val module = new CompileToGP().transformModule(desugared)
    if (CompilerFlags.DEBUGMODE) {
      println(s"Intermediate Representation")
      println(module)
    }
    module
  }
}
