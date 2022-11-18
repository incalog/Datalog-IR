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
    """, ObjectOptions())

  lazy val addRunner: ObjectOrientedRunner = {
    new ObjectOrientedDatalog(addModule).runner("A", "add")
  }

  test("Add Example") {
    val resRel = addRunner.run(ObjectOrientedInput(q"1", q"2"))
    val resRel2 = addRunner.run(ObjectOrientedInput(q"1", q"3"))

    println(resRel)
    addRunner.readAll.foreach { rel =>
      println()
      println(rel.asTable)
    }

    println()
    println()
    println()
    println(resRel2)
    addRunner.readAll.foreach { rel =>
      println()
      println(rel.asTable)
    }

    assert(resRel.toSet.head == 3)
    assert(resRel2.toSet.head == 4)
  }
}
