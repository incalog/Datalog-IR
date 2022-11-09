package inca.frontend.objectoriented.runner

import inca.compiler.{CompiledModule, Compiler}
import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class ObjectOrientedTest extends AnyFunSuite {
  lazy val addModule: CompiledObjectModule = Compiler.compileObject(
    """module AddTest

    class A {
      @main
      def add(a: Int, b: Int): Int = {
        return a + b
      }
    }
    """.stripMargin, ObjectOptions())

  lazy val addRunner: ObjectOrientedRunner = {
    new ObjectOrientedRunnerFactory(addModule).runner("A", "add")
  }

  test("Add Example") {
    val resRel = addRunner.run(ObjectOrientedInput(q"1", q"2"))
    assert(resRel.toSet.head == 3)
  }
}
