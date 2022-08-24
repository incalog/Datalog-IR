package inca.frontend.objectoriented.lowering

import inca.backend.ir.Datalog
import inca.frontend.objectoriented.core._

object GenerateDatalog {
  def transformModule(module: Module): Datalog.Module =
    new GenerateDatalog(module).transModule()

  def transformModules(modules: Seq[Module]): Seq[Datalog.Module] =
    modules.map(transformModule)
}

class GenerateDatalog(module: Module) {
  def transModule(): Datalog.Module = {
    ???
  }
}