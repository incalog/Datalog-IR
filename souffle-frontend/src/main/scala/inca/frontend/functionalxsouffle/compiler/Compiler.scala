package inca.frontend.functionalxsouffle.compiler

import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.parser.{Parser => FunctionalParser}
import inca.frontend.souffle.parser.{Parser => SouffleParser}

object Compiler {

  def compileFunctionalAndSouffle(fun: String, souffle: String,
                        compilerOptions: FunctionalOptions): CompiledFunctionalAndSouffleModule = {
    val funParsed = FunctionalParser.parse(fun)
    val souffleParsed = SouffleParser.parse(souffle)
    CompiledFunctionalAndSouffleModule(funParsed, souffleParsed, compilerOptions)
  }
}
