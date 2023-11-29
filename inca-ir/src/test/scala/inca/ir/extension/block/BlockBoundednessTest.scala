package inca.ir.extension.block

import inca.ir.*
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.not.Not
import inca.ir.extension.{arithmetic, block, not}
import inca.ir.typing.{BaseIRTypechecker, TypeErrorException, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike

class BlockBoundednessTest extends AnyFunSuiteLike:

  def module(relations: Relation*)(using typechecker: BaseIRTypechecker): Module =
    val mod = Module("M", BaseIR.language, relations)
    println(mod)
    typechecker.checkModule(mod)
    mod

  test("eq block binds contained terms") {
    implicit val typechecker = new BaseIRTypechecker with block.Typechecker with arithmetic.Typechecker {}
    module(
      Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
        Eq(Block(Seq(Eq(IntNum(0), IntNum(0))), Var("p")), IntNum(1))
      ))))
    )

    module(
      Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
        Eq(IntNum(1), Block(Seq(Eq(IntNum(0), IntNum(0))), Var("p")))
      ))))
    )

    module(
      Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
        Eq(Block(Seq(Eq(IntNum(0), IntNum(0))), IntNum(5)), Block(Seq(Eq(IntNum(0), IntNum(0))), Var("p")))
      ))))
    )
  }

  test("eq block binds contained terms 2") {
    assertThrows[TypeErrorException] {
      implicit val typechecker = new BaseIRTypechecker with block.Typechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
          Eq(Block(Seq(Eq(IntNum(0), IntNum(0))), Var("p")), Block(Seq(Eq(IntNum(0), IntNum(0))), Var("p")))
        ))))
      )
    }

    assertThrows[TypeErrorException] {
      implicit val typechecker = new BaseIRTypechecker with block.Typechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Eq(Block(Seq(Eq(IntNum(0), IntNum(0))), Var("p1")), Block(Seq(Eq(IntNum(0), IntNum(0))), Var("p2")))
        ))))
      )
    }
  }

