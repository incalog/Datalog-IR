package inca.frontend.objectoriented.datalog

import inca.compiler.{CompiledModule, Compiler}
import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class ObjectOrientedTest extends AnyFunSuite {
  lazy val addModule: CompiledObjectModule = Compiler.compileObject(
    """module AddTest

    class MonoAdd {
      var state : Int = 0
      def add(a : Int) : Unit = {
        this.state = this.state + a
      }
      def result() : Int = {
        return this.state
      }
    }

    class A {
      @main
      def main(): Unit = {
        val a : MonoAdd = new MonoAdd()
        a.add(2)
      }
    }
    """, ObjectOptions())

  test("Add Example") {
    val datalog = new ObjectOrientedDatalog(addModule)
    val resRel = datalog.run("A", "main")

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
