package inca.frontend.functional.typecheck

import inca.frontend.functional.core.Module
import inca.frontend.functional.parser.Parser
import inca.frontend.functional.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite {

  def newTypechecker(): Typechecker = new Typechecker { }

  def checkModule(mod: String): Unit = {
    checkModule(Parser.parse(mod))
  }

  def checkModule(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    assert(checker.getErrors.isEmpty, s"Found type errors ${checker.getErrors}")
  }

  def checkModuleErrors(mod: String): Unit = {
    checkModuleErrors(Parser.parse(mod))
  }

  def checkModuleErrors(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    assert(checker.getErrors.nonEmpty, s"Expected type errors, but none found")
  }

  test("base example 1") {
    val code = FileUtil.readFile("functional/unittests/Base1.finca")
    checkModule(code)
  }

  test("base example 2") {
    val code = FileUtil.readFile("functional/unittests/Base2.finca")
    checkModule(code)
  }

  test("var example") {
    val code = FileUtil.readFile("functional/unittests/Var.finca")
    checkModule(code)
  }

  test("if example") {
    val code = FileUtil.readFile("functional/unittests/If.finca")
    checkModule(code)
  }

  test("if example 2") {
    val code = FileUtil.readFile("functional/unittests/If2.finca")
    checkModule(code)
  }

  test("inc example") {
    val code = FileUtil.readFile("functional/unittests/Inc.finca")
    checkModule(code)
  }

  test("fact example") {
    val code = FileUtil.readFile("functional/unittests/Fact.finca")
    checkModule(code)
  }

  test("plus example") {
    val code = FileUtil.readFile("functional/unittests/Plus.finca")
    checkModule(code)
  }

  test("plus real example") {
    val code = FileUtil.readFile("functional/unittests/PlusReal.finca")
    checkModule(code)
  }

  test("set constants") {
    val code = FileUtil.readFile("functional/unittests/SetConst.finca")
    checkModule(code)
  }

  test("set operations") {
    val code = FileUtil.readFile("functional/unittests/SetOps.finca")
    checkModule(code)
  }

  test("set intersection") {
    val code = FileUtil.readFile("functional/unittests/SetIntersection.finca")
    checkModule(code)
  }

  test("fold int") {
    val code = FileUtil.readFile("functional/unittests/FoldInt.finca")
    checkModule(code)
  }
  test("fold adt") {
    val code = FileUtil.readFile("functional/unittests/FoldADT.finca")
    checkModule(code)
  }

  test("applyFun") {
    val code = FileUtil.readFile("functional/higherorder/Apply.finca")
    checkModule(code)
  }

  test("lambda") {
    val code = FileUtil.readFile("functional/higherorder/Lambda.finca")
    checkModule(code)
  }

  test("lambdaHigherOrder") {
    val code = FileUtil.readFile("functional/higherorder/LambdaHigherOrder.finca")
    checkModule(code)
  }

  test("composeFun") {
    val code = FileUtil.readFile("functional/higherorder/ComposeFun.finca")
    checkModule(code)
  }

  test("composeLambdas") {
    val code = FileUtil.readFile("functional/higherorder/ComposeLambda.finca")
    checkModule(code)
  }

  test("transitive") {
    val code = FileUtil.readFile("functional/higherorder/Transitive.finca")
    checkModule(code)
  }
  

  test("available expressions") {
    val code = FileUtil.readFile("functional/controlflow/AvailableExpressions.finca")
    checkModule(code)
  }

  test("reaching definitions") {
    val code = FileUtil.readFile("functional/controlflow/ReachingDefinition.finca")
    checkModule(code)
  }

  test("intervals") {
    val code = FileUtil.readFile("functional/controlflow/Interval.finca")
    checkModule(code)
  }

  test("aeval") {
    val code = FileUtil.readFile("functional/controlflow/AEval.finca")
    checkModule(code)
  }

  test("parent call for adt") {
    val code =
      s"""module Test
         |data Nat = Zero() | Succ(Nat)
         |
         |@main def main(): Option[Any] = parent(Zero())
         |""".stripMargin
    checkModule(code)
  }

  test("parent call wrong number of args") {
    val code =
      s"""module Test
         |data Nat = Zero() | Succ(Nat)
         |
         |@main def main(): Option[Any] = parent(Zero(), Succ(Zero()))
         |""".stripMargin
    checkModuleErrors(code)
  }

  test("parent call for non adt") {
    val code =
      s"""module Test
         |data Nat = Zero() | Succ(Nat)
         |
         |@main def main(): Option[Any] = parent(1)
         |""".stripMargin
    checkModuleErrors(code)
  }

  test("type cast for adt") {
    val code =
      s"""module Test
         |data Nat = Zero() | Succ(Nat)
         |
         |@main def main(x: Any): Nat = x.as[Nat]
         |""".stripMargin
    checkModule(code)
  }

  test("type cast for non adt") {
    val code =
      s"""module Test
         |data Nat = Zero() | Succ(Nat)
         |
         |@main def main(x: Any): Int = x.as[Int]
         |""".stripMargin
    checkModuleErrors(code)
  }

}
