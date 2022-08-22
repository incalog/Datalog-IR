package inca.frontend.objectoriented.typecheck

import inca.frontend.objectoriented.core.Module
import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite {

  def newTypechecker(): Typechecker = new Typechecker {}

  def checkModule(mod: String): Unit = {
    checkModule(Parser.parse(mod))
  }

  def checkModule(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    checker.getErrors.foreach(println)
    assert(checker.getErrors.isEmpty, s"Found type errors ${checker.getErrors}")
  }

  def checkModuleErrors(mod: String): Unit = {
    checkModuleErrors(Parser.parse(mod))
  }

  def checkModuleErrors(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    checker.getErrors.foreach(println)
    assert(checker.getErrors.nonEmpty, s"Expected type errors, but none found")
  }


  test("Simple syntax test") {
    val code = FileUtil.readFile("objectoriented/unittests/success/Playground.oinca")
    checkModule(code)
  }

  test("BinaryTree") {
    val code = FileUtil.readFile("objectoriented/unittests/success/BinaryTree.oinca")
    checkModule(code)
  }
}