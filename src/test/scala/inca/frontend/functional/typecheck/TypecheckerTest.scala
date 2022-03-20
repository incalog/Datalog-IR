package inca.frontend.functional.typecheck

import inca.compiler.source.SourceString
import inca.examples.functional.AST
import inca.examples.functional.Code
import inca.examples.functional.ControlDataFlow
import inca.examples.functional.HigherOrder
import inca.frontend.functional.core.Module
import inca.frontend.functional.parser.Parser
import inca.frontend.functional.typechecker.Typechecker
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite {

  def newTypechecker(): Typechecker = new Typechecker {}

  def checkModule(mod: String): Unit = {
    checkModule(Parser.parse(SourceString(mod)))
  }

  def checkModule(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    assert(checker.getErrors.isEmpty, s"Found type errors ${checker.getErrors}")
  }

  def checkModuleErrors(mod: String): Unit = {
    checkModuleErrors(Parser.parse(SourceString(mod)))
  }

  def checkModuleErrors(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    assert(checker.getErrors.nonEmpty, s"Expected type errors, but none found")
  }

  test("base example") {
    checkModule(AST.baseExample)
  }

  test("base example 2") {
    checkModule(AST.baseExample2)
  }

  test("var example") {
    checkModule(AST.varExample)
  }

  test("if example") {
    checkModule(AST.ifExample)
  }

  test("if example 2") {
    checkModule(AST.ifExample2)
  }

  test("inc example") {
    checkModule(AST.incModule)
  }

  test("fact example") {
    checkModule(AST.factModule)
  }

  test("plus example") {
    checkModule(AST.plusModule)
  }

  test("plus real example") {
    checkModule(AST.plusRealModule)
  }

  test("set constants") {
    checkModule(Code.setConstModule)
  }

  test("set operations") {
    checkModule(Code.setOperationsModule)
  }

  test("simple fold") {
    checkModule(Code.simpleFoldModule)
  }

  test("applyFun") {
    checkModule(HigherOrder.applyFun)
  }

  test("lambda") {
    checkModule(HigherOrder.lambda)
  }

  test("lambdaHigherOrder") {
    checkModule(HigherOrder.lambdaHigherOrder)
  }

  test("composeFun") {
    checkModule(HigherOrder.composeFun)
  }

  test("composeLambdas") {
    checkModule(HigherOrder.composeLambdas)
  }

  test("transitive") {
    checkModule(HigherOrder.transitive)
  }

  test("cflow") {
    checkModule(ControlDataFlow.cflowModule)
  }

  test("available expressions") {
    checkModule(ControlDataFlow.AEModule)
  }

  test("reaching definitions") {
    checkModule(ControlDataFlow.RDmodule)
  }

  test("intervals") {
    checkModule(ControlDataFlow.IntervalModule)
  }

  test("aeval") {
    checkModule(ControlDataFlow.AEvalModule)
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

//  test("emptiness check 1") {
//    val module = Frontend.Core.parseModule(Code.module(
//      s"""def foo(i: Int): Option[Int] = bar(i)
//         |@main def bar(i: Int): Option[Int] = foo(i) match {
//         |  case None => None
//         |  case Some(j) => Some(i)
//         |}
//         |""".stripMargin
//    )).get.value
//    checkModule(module)
//    import scala.meta._
//    val fun = Executor.loadFunction(module.prettyprint(""))
//    assert(fun.execute("bar_bf", Seq(q"12")) == fun.results(Seq()))
//  }
//
//  test("emptiness check 2") {
//    val module = Frontend.Core.parseModule(Code.module(
//      s"""def foo(i: Int): Option[Int] = bar(i)
//         |def bar(i: Int): Option[Int] = foo(i) match {
//         |  case None => Some(0)
//         |  case Some(j) => Some(i)
//         |}
//         |""".stripMargin
//    )).get.value
//    checkModuleErrors(module)
//  }
//
//  test("emptiness check 2b") {
//    val module = Frontend.Core.parseModule(Code.module(
//      s"""def foo(i: Int): Option[Int] = Some(1)
//         |def irr(i: Int): Option[Int] = bar(i)
//         |@main def bar(i: Int): Option[Int] = let x = irr(i) in foo(i) match {
//         |  case None => Some(0)
//         |  case Some(j) => Some(i)
//         |}
//         |""".stripMargin
//    )).get.value
//    checkModuleErrors(module)
////    import scala.meta._
////    val fun = Executor.loadFunction(module.prettyprint(""))
////    println(fun.compiled.optimized)
////    assert(fun.execute("bar_bf", Seq(q"12")) == fun.results(Seq()))
//  }
//
//  test("emptiness check 3") {
//    val module = Frontend.Core.parseModule(Code.module(
//      s"""def foo(i: Int): Option[Int] = bar(i)
//         |def baz(i: Int): Option[Int] = foo(i)
//         |def bar(i: Int): Option[Int] = baz(i) match {
//         |  case None => Some(0)
//         |  case Some(j) => Some(i)
//         |}
//         |""".stripMargin
//    )).get.value
//    checkModuleErrors(module)
//  }
//
//  test("emptiness check 4") {
//    val module = Frontend.Core.parseModule(Code.module(
//      s"""def foo(i: Int): Option[Int] = Some(1)
//         |def baz(i: Int): Option[Int] = Some(5)
//         |@main def bar(i: Int): Option[Int] = baz(i) match {
//         |  case None => Some(0)
//         |  case Some(j) => let x = bar(i) in bar(i)
//         |}
//         |""".stripMargin
//    )).get.value
//    checkModule(module)
//    import scala.meta._
//    val fun = Executor.loadFunction(module.prettyprint(""))
//    println(fun.compiled.optimized)
//    assert(fun.execute("bar_bf", Seq(q"12")) == fun.results(Seq()))
//  }
//
//  test("emptiness check 5") {
//    val module = Frontend.Core.parseModule(Code.module(
//      s"""@main def foo(i: Int): Option[Int] = let x = foo(i) in bar(i)
//         |def baz(i: Int): Option[Int] = Some(5)
//         |def bar(i: Int): Option[Int] = baz(i) match {
//         |  case None => Some(0)
//         |  case Some(j) => Some(i)
//         |}
//         |""".stripMargin
//    )).get.value
//    checkModuleErrors(module)
////    import scala.meta._
////    val fun = Executor.loadFunction(module.prettyprint(""))
////    println(fun.compiled.optimized)
////    assert(fun.execute("foo_bf", Seq(q"12")) == fun.results(Seq()))
//  }
}
