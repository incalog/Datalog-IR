package inca.frontend.functional.lowering

import inca.compiler.Compiler
import inca.frontend.functional.compiler.FunctionalOptions
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite


class GenerateDatalogTest extends AnyFunSuite {

  val options: FunctionalOptions = FunctionalOptions()

  test("base example 1") {
    val code = FileUtil.readFile("functional/unittests/Base1.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("base example 2") {
    val code = FileUtil.readFile("functional/unittests/Base2.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }


  test("var example") {
    val code = FileUtil.readFile("functional/unittests/Var.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("if example") {
    val code = FileUtil.readFile("functional/unittests/If.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("if example 2") {
    val code = FileUtil.readFile("functional/unittests/If2.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("inc example") {
    val code = FileUtil.readFile("functional/unittests/Inc.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("fact example") {
    val code = FileUtil.readFile("functional/unittests/Fact.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("plus example") {
    val code = FileUtil.readFile("functional/unittests/Plus.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("plus real example") {
    val code = FileUtil.readFile("functional/unittests/PlusReal.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("set constants") {
    val code = FileUtil.readFile("functional/unittests/SetConst.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("set operations") {
    val code = FileUtil.readFile("functional/unittests/SetOps.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("applyFun") {
    val code = FileUtil.readFile("functional/higherorder/Apply.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("lambda") {
    val code = FileUtil.readFile("functional/higherorder/Lambda.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("lambdaHigherOrder") {
    val code = FileUtil.readFile("functional/higherorder/LambdaHigherOrder.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("composeFun") {
    val code = FileUtil.readFile("functional/higherorder/ComposeFun.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }

  test("composeLambdas") {
    val code = FileUtil.readFile("functional/higherorder/ComposeLambda.finca")
    val result = Compiler.compileFunctional(code, options).ir
    println(result)
  }
}
