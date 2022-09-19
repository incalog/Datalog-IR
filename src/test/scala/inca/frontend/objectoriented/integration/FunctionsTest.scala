package inca.frontend.objectoriented.integration

import inca.frontend.objectoriented.executor.ObjectExecutor
import inca.util.FileUtil.readFile
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.executor.ObjectExecutor.TypeCastException
import org.scalatest.Assertion

import scala.meta.{Term, XtensionQuasiquoteTerm}

class FunctionsTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions()

  private def performSingleOutputValueTest[O](file: String, main: String, input: Seq[Term], expectedResult: O): Assertion = {
    val code = readFile(s"objectoriented/unittests/$file.oinca")
    val fun = ObjectExecutor.loadFunction(code, options)
    val result = fun.execute(main, input)
    fun.printAllMatches()
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

  test("FieldAccess Example") {
    performSingleOutputValueTest("FieldAccess", "Fraction$main", Seq(q"16", q"8"), 2)
  }

  test("FieldAccessNested Example") {
    performSingleOutputValueTest("FieldAccessNested", "A$main", Seq(), 3)
  }

  test("InstanceOf Example") {
    performSingleOutputValueTest("InstanceOf", "A$main", Seq(), true)
  }

  test("TypeCast Example") {
    performSingleOutputValueTest("TypeCast", "A$main", Seq(), true)
  }

  test("TypeCastFail Example") {
    val caught = intercept[TypeCastException] {
      performSingleOutputValueTest("TypeCastFail", "A$main", Seq(), Seq())
    }
    assert(caught.typ == "B")
    assert(caught.obj.typ == "A")
  }

  test("DynamicDispatch Example") {
    performSingleOutputValueTest("DynamicDispatch", "A$main", Seq(), "BBC")
  }

  test("MethodInheritance Example") {
    performSingleOutputValueTest("MethodInheritance", "A$main", Seq(), 3)
  }

  test("BinaryTree Sum") {
    performSingleOutputValueTest("BinaryTree3", "DefinedNode$main", Seq(), 20)
  }

  test("Plus Example") {
    performSingleOutputValueTest("Plus", "Nat$main", Seq(), 5)
  }
}