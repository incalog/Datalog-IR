package inca.frontend.datalog

import inca.frontend.Frontend
import inca.frontend.datalog.compiler.{CompiledDatalogModule, DatalogOptions}

object DatalogFrontend extends Frontend[syntax.Module, DatalogOptions] {

  override def compile(source: String, opt: DatalogOptions): CompiledDatalogModule =
    CompiledDatalogModule(source, opt)
  override def compile(module: syntax.Module, opt: DatalogOptions): CompiledDatalogModule =
    CompiledDatalogModule(module, opt)
}
