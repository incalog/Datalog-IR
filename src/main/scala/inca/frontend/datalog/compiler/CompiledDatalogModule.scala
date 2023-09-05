package inca.frontend.datalog.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.compiler.source.SourceLocation
import inca.compiler.CompiledModule
import inca.compiler.CompilerFlags
import inca.frontend.datalog.lowering._
import inca.frontend.datalog.syntax.Module
import inca.frontend.datalog.syntax.Parser
import inca.frontend.datalog.typechecker.Typechecker
import inca.runtime.context.DataModel

object CompiledDatalogModule {
  def apply(source: String, options: DatalogOptions): CompiledDatalogModule = {
    val tree = Parser.parse(source)
    CompiledDatalogModule(tree, options)
  }

}

case class CompiledDatalogModule(source: Module, options: DatalogOptions) extends CompiledModule {
  outer =>

  override def name: Name = source.name.name

  override def sourceLocation: SourceLocation = source.name

  lazy val dataModel: DataModel = {
    val res = new GenerateDataModel(source).transModule()
    res
  }

  lazy val typer: Typechecker = new Typechecker {
    override val dataModel: DataModel = outer.dataModel
  }

  lazy val typed: Module = {
    typer.typecheck(source)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    source
  }

  lazy val ir: Datalog.Module = {
    val module = new GenerateIR(typed).transModule()
    if (CompilerFlags.DEBUGMODE) {
      println(s"Intermediate Representation")
      println(module)
    }
    module
  }
}
