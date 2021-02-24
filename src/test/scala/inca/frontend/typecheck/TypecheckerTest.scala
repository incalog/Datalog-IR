package inca.frontend.typecheck

import inca.Executor
import inca.examples.{AST, Code}
import inca.frontend.Frontend
import inca.frontend.core.Module
import inca.frontend.typechecker.Typechecker
import org.scalatest.funsuite.AnyFunSuite

class TypecheckerTest extends AnyFunSuite {

  def newTypechecker(): Typechecker = new Typechecker { }

  def checkModule(mod: Module): Unit = {
    val checker = newTypechecker()
    checker.typecheck(mod)
    assert(checker.getErrors.isEmpty, s"Found type errors ${checker.getErrors}")
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

  test("emptiness check 1") {
    val module = Frontend.Core.parseModule(Code.module(
      s"""def foo(i: `Int`): Option[`Int`] = bar(i)
         |@main def bar(i: `Int`): Option[`Int`] = foo(i) match {
         |  case None => None
         |  case Some(j) => Some(i)
         |}
         |""".stripMargin
    )).get.value
    checkModule(module)
    import scala.meta._
    val fun = Executor.loadFunction(module.prettyprint(""))
    assert(fun.execute("bar_bf", Seq(q"12")) == fun.results(Seq()))
  }

  test("emptiness check 2") {
    val module = Frontend.Core.parseModule(Code.module(
      s"""def foo(i: `Int`): Option[`Int`] = bar(i)
         |def bar(i: `Int`): Option[`Int`] = foo(i) match {
         |  case None => Some(0)
         |  case Some(j) => Some(i)
         |}
         |""".stripMargin
    )).get.value
    checkModuleErrors(module)
  }

  test("emptiness check 3") {
    val module = Frontend.Core.parseModule(Code.module(
      s"""def foo(i: `Int`): Option[`Int`] = bar(i)
         |def baz(i: `Int`): Option[`Int`] = foo(i)
         |def bar(i: `Int`): Option[`Int`] = baz(i) match {
         |  case None => Some(0)
         |  case Some(j) => Some(i)
         |}
         |""".stripMargin
    )).get.value
    checkModuleErrors(module)
  }

  test("emptiness check 4") {
    val module = Frontend.Core.parseModule(Code.module(
      s"""def foo(i: `Int`): Option[`Int`] = Some(1)
         |def baz(i: `Int`): Option[`Int`] = Some(5)
         |@main def bar(i: `Int`): Option[`Int`] = baz(i) match {
         |  case None => Some(0)
         |  case Some(j) => let x = bar(i) in bar(i)
         |}
         |""".stripMargin
    )).get.value
    checkModuleErrors(module)
//    import scala.meta._
//    val fun = Executor.loadFunction(module.prettyprint(""))
//    println(fun.compiled.optimized)
//    assert(fun.execute("bar_bf", Seq(q"12")) == fun.results(Seq()))
  }

  test("emptiness check 5") {
    val module = Frontend.Core.parseModule(Code.module(
      s"""@main def foo(i: `Int`): Option[`Int`] = let x = foo(i) in bar(i)
         |def baz(i: `Int`): Option[`Int`] = Some(5)
         |def bar(i: `Int`): Option[`Int`] = baz(i) match {
         |  case None => Some(0)
         |  case Some(j) => Some(i)
         |}
         |""".stripMargin
    )).get.value
    checkModuleErrors(module)
//    import scala.meta._
//    val fun = Executor.loadFunction(module.prettyprint(""))
//    println(fun.compiled.optimized)
//    assert(fun.execute("foo_bf", Seq(q"12")) == fun.results(Seq()))
  }
}
