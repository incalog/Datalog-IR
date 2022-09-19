package inca.frontend.objectoriented.lowering

import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite


class GenerateDatalogTest extends AnyFunSuite {

  val options: ObjectOptions = ObjectOptions()

  def printIr(filename: String): Unit = {
    val code = FileUtil.readFile(s"objectoriented/unittests/$filename.oinca")
    val result = Compiler.compileObject(code, options).ir
    print(result)
  }

  test("Base 1") {
    printIr("Base1")
  }

  test("Base 2") {
    printIr("Base2")
  }

  test("Base 3") {
    printIr("Base3")
  }

  test("Plus") {
    printIr("Plus")
  }
}
