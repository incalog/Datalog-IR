package inca.ir.typing

import inca.ir.*
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.IntNum
import inca.ir.typing.TypeCheckerDefinitional
import inca.ir.typing.TypeCheckerDefinitional.TypeError
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.util.Try

class TypeCheckerDefinitionalTest extends AnyFunSuiteLike:

  def module(relations: Relation*): Module =
    val mod = Module("M", BaseIR.language, relations)
    println(mod)
    val result = Try(TypeCheckerDefinitional.checkModule(mod))
    if (result.isFailure) {
      val failed = result.failed.get
      println(failed.getMessage)
      throw failed
    }
    mod

  test("no body, no binding") {
    module(Relation("R", Seq(Param("p", TAny)), Seq()))
  }

  test("unbound param") {
    assertThrows[TypeError](
      module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq()))))
    )
  }

  test("bound param") {
    module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(Var("p"), IntNum(1))
    )))))
    module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(IntNum(1), Var("p"))
    )))))
  }

  test("unbound param 2") {
    assertThrows[TypeError](
      module(Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Eq(Var("p1"), IntNum(1))
      )))))
    )
  }

  test("bound param 2") {
    module(Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
      Eq(Var("p1"), IntNum(1)),
      Eq(Var("p1"), Var("p2"))
    )))))
  }

  test("eq binds locals") {
    module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(Var("x"), IntNum(1)),
      Eq(Var("p"), Var("x"))
    )))))
    module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(IntNum(1), Var("x")),
      Eq(Var("x"), Var("p"))
    )))))
  }


    test("unbound variable in neq test") {
    assertThrows[TypeError] {
      module(Relation("R", Seq(), Seq(Body(Seq(
        Neq(Var("x"), Var("y"))
      )))))
    }
    assertThrows[TypeError]{
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(Var("x"), IntNum(0)),
        Neq(Var("x"), Var("y"))
      )))))
    }
    assertThrows[TypeError]{
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(IntNum(0), Var("y")),
        Neq(Var("x"), Var("y"))
      )))))
    }
    module(Relation("R", Seq(), Seq(Body(Seq(
      Eq(Var("x"), IntNum(0)),
      Eq(IntNum(0), Var("y")),
      Neq(Var("x"), Var("y"))
    )))))
  }

  test("call binds arguments") {
    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Call("T", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
  }

  test("neg call requires bound arguments") {
    assertThrows[TypeError] {
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          NegCall("T", Seq(Var("p1"), Var("p2")))
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[TypeError] {
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          NegCall("T", Seq(Var("p1"), IntNum(2)))
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }
  }

  // TODO: test extensional calls