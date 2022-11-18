package inca.frontend.objectoriented.datalog

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

  test("Add Example") {
    val datalog = new ObjectOrientedDatalog(addModule)
    val resRel = datalog.run("A", "add", q"1", q"2")

    println(resRel)
    datalog.readAll.foreach { rel =>
      println()
      println(rel.asTable)
    }

    println()
    println()

    val resRel2 = datalog.run("A", "add", q"1", q"3")
    println(resRel2)
    datalog.readAll.foreach { rel =>
      println()
      println(rel.asTable)
    }

    assert(resRel.toSet.head == 3)
    assert(resRel2.toSet.head == 4)
  }
}
