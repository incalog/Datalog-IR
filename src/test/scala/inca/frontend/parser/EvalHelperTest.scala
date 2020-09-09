package inca.frontend.parser

import inca.frontend.core.Core.Name
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.{Term, XtensionParseInputLike}

class EvalHelperTest extends AnyFunSuite {

  test("test freeVars Name") {
    checkVars("x", Set("x"))
  }

  test("test freeVars select") {
    checkVars("x.name", Set("x"))
  }

  test("test freeVars apply unary") {
    checkVars("!ten", Set("ten"))
  }

  test("test freeVars apply infix") {
    checkVars("x + y", Set("x", "y"))
  }

  test("test freeVars return") {
    checkVars("return value", Set("value"))
  }

  test("test freeVars ascribe") {
    checkVars("value.asInstanceOf[Int]", Set("value"))
  }

  test("test freeVars throw") {
    checkVars("throw value", Set("value"))
  }

  test("test freeVars apply") {
    checkVars("fun(arg)", Set("fun", "arg"))
  }

  test("test freeVars tuple") {
    checkVars("(val1, val2, val3)", Set("val1", "val2", "val3"))
  }

  private def checkVars(code: String, expectedFree: Set[Name]): Unit = {
    val tree = code.parse[Term].get
    val free = EvalHelper.freeVars(tree).toSet
    assert(free == expectedFree)
  }

}
