package inca.frontend.functional.executor

import dotty.tools.io.File
import inca.frontend.functional.compile.{CompiledFunctionalModule, GenerateDatalog}
import inca.frontend.functional.executor.FunctionalExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
import inca.frontend.functional.syntax.*

class ExecutorTest extends AnyFunSuite {
  test("Base 1 ") {
    val file = Source.fromResource("functional/unittests/Base1.finca")
    val code = file.getLines().mkString("\n")
    file.close()

    val compiler = new GenerateDatalog
    val module = Parser.parseModule(code)
    val compiled = CompiledFunctionalModule(module)

    println(compiled.psystemSource)

    val loaded = FunctionalExecutor.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    println(res.asTable)
  }
}
