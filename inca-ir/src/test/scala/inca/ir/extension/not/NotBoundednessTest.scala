package inca.ir.extension.not

import inca.ir.*
import inca.ir.extension.{arithmetic, not}
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.not.Not
import inca.ir.typing.{BaseIRTypechecker, TypeErrorException, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike

class NotBoundednessTest extends AnyFunSuiteLike:

  def module(relations: Relation*)(using typechecker: BaseIRTypechecker with not.Typechecker): Module =
    val mod = Module("M", BaseIR.language, relations)
    println(mod)
    typechecker.typecheck(mod)
    mod

  test("not call requires bound arguments") {
    assertThrows[TypeErrorException] {
      implicit val typechecker: BaseIRTypechecker with not.Typechecker = new BaseIRTypechecker with not.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Not(Call("T", Seq(Var("p1"), Var("p2"))))
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }
  }

  test("not neg call binds arguments") {
    implicit val typechecker: BaseIRTypechecker with not.Typechecker = new BaseIRTypechecker with not.Typechecker {}
    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(Call("T", Seq(Var("p1"), Var("p2")), true))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
  }

