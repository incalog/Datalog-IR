package inca.frontend.objectoriented.integration

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.util.printer.DatalogPrinter
import inca.frontend.objectoriented.executor.ObjectExecutor
import inca.util.FileUtil.readFile
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.objectoriented.compiler.ObjectOptions

class FunctionsTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions(Seq())

  test("Plus Example") {
    val code = readFile("objectoriented/unittests/Plus.oinca")
    val fun = ObjectExecutor.loadFunction(code, options)
    val result = fun.execute("Nat$main", Seq())
    fun.printAllMatches()
    println(result)
  }
}