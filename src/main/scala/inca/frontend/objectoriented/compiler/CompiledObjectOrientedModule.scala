package inca.frontend.objectoriented.compiler

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.frontend.objectoriented.core.Module
import inca.compiler.{CompiledModule, SourceLocation}
import inca.runtime.context.DataModel

case class CompiledObjectOrientedModule(fun: Module, options: ObjectOrientedOptions) extends CompiledModule {

  override def name: Name = fun.name.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typed: Module = {
    // TODO: Implement a typechecker
    fun
  }

  lazy val monoModule: Module = {
    // TODO: Implment Monomorph
    fun
  }

  lazy val coreModule: Module = {
    // TODO: Implement Defunctionalize
    fun
  }

  // TODO: Generate Datalog
  lazy val ir: Datalog.Module = {
    Datalog.Module(name = name, imports = List(), pats = List(), scalaContent = List())
  }

  // TODO: Generate DataModel
  lazy val dataModel: DataModel = {
    new DataModel()
  }
}
