package inca.frontend.objectoriented.integration

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.util.printer.DatalogPrinter
import inca.frontend.objectoriented.executor.ObjectExecutor
import inca.util.FileUtil.readFile
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.objectoriented.compiler.ObjectOptions
import org.scalatest.Assertion

import scala.meta.{Term, XtensionQuasiquoteTerm}

class FunctionsTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions(Seq())

  private def performSingleOutputValueTest[O](file: String, main: String, input: Seq[Term], expectedResult: O): Assertion = {
    val code = readFile(s"objectoriented/unittests/$file.oinca")
    val fun = ObjectExecutor.loadFunction(code, options)
    val result = fun.execute(main, input)
    assertResult(expectedResult)(result.res.head.head)
  }

  test("Base 1 Example") {
    performSingleOutputValueTest("Base1", "Base1$main", Seq(), 43)
  }

  test("Base 2 Example") {
    performSingleOutputValueTest("Base2", "Base2$main", Seq(), 43)
  }

  test("Base 3 Example") {
    performSingleOutputValueTest("Base3", "Base3$main", Seq(), 43)
  }

  test("Factorial Example") {
    performSingleOutputValueTest("Fact", "Factorial$main", Seq(q"5"), 120)
  }

  test("Fibonacci Example") {
    performSingleOutputValueTest("Fib", "Fibonacci$main", Seq(q"11"), 89)
  }

  test("InstanceOf Example") {
    performSingleOutputValueTest("InstanceOf", "A$main", Seq(), true)
  }

  test("TypeCast Example") {
    performSingleOutputValueTest("TypeCast", "A$main", Seq(), true)
  }

  test("Plus Example") {
    val code = readFile("objectoriented/unittests/Plus.oinca")
    val fun = ObjectExecutor.loadFunction(code, options)
    val result = fun.execute("Nat$main", Seq())
    fun.printAllMatches()
    println(result)
  }
}