package inca.frontend.objectoriented.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.backend.ir.util.printer.DatalogPrinter
import inca.compiler.{CompiledModule, CompilerFlags, SourceLocation}
import inca.frontend.objectoriented.analyze.AbstractSyntaxTree
import inca.frontend.objectoriented.core.Module
import inca.frontend.objectoriented.lowering.{AddMissingDefinitions, Defunctionalize, GenerateDataModel, GenerateDatalog, StaticSingleAssignment}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.runtime.context.DataModel

case class CompiledObjectModule(fun: Module, options: ObjectOptions) extends CompiledModule {

  override def name: Name = fun.name.raw

  override def sourceLocation: SourceLocation = fun.name

  lazy val typer: Typechecker = new Typechecker {}

  lazy val completed: Module = {
    if (CompilerFlags.DEBUGMODE) {
      println("Module")
      println(fun)

      if (CompilerFlags.DebugConfig.AST) {
        println()
        println("Module - AST")
        println(new AbstractSyntaxTree(fun).toGraphViz)
      }
    }

    AddMissingDefinitions.transformModule(fun)
  }

  lazy val typed: Module = {
    if (CompilerFlags.DEBUGMODE) {
      println("Typed Module")
      println(completed)

      if (CompilerFlags.DebugConfig.AST) {
        println()
        println("Typed Module - AST")
        println(new AbstractSyntaxTree(completed).toGraphViz)
      }
    }

    typer.typecheck(completed)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()

    completed
  }

  lazy val ssaModule: Module = {
    val module = StaticSingleAssignment.transformModule(typed)

    if (CompilerFlags.DEBUGMODE) {
      println(s"SSA Module")
      println(module)

      if (CompilerFlags.DebugConfig.AST) {
        println()
        println("SSA Module - AST")
        println(new AbstractSyntaxTree(module).toGraphViz)
      }
    }

    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()

    module
  }

  lazy val coreModule: Module = {
    val dataModel = new GenerateDataModel(ssaModule)
    val module = Defunctionalize.transformModule(ssaModule, dataModel.transModule())

    if (CompilerFlags.DEBUGMODE) {
      println(s"Core Module")
      println(module)

      if (CompilerFlags.DebugConfig.AST) {
        println()
        println("Defun Module - AST")
        println(new AbstractSyntaxTree(module).toGraphViz)
      }
    }

    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()

    module
  }

  lazy val ir: Datalog.Module = {
    val module = new GenerateDatalog(coreModule).transModule()

    println(s"Intermediate Representation")
    println(module)

    if (CompilerFlags.DEBUGMODE) {
      println(s"Intermediate Representation")
      println(module)
    }
    module
  }

  lazy val dataModel: DataModel = {
    new GenerateDataModel(coreModule).transModule()
  }
}
