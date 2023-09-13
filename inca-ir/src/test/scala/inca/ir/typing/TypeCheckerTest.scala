package inca.ir.typing

import inca.ir.*
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.not
import inca.ir.extension.not.Not
import inca.ir.extension.demand
import inca.ir.extension.demand.Demand
import inca.ir.typing.{BaseIRTypechecker, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike

class TypeCheckerTest extends AnyFunSuiteLike:

  def module(relations: Relation*)(using typechecker: BaseIRTypechecker): Module =
    val mod = Module("M", BaseIR.language, relations)
    try typechecker.typecheck(mod)
    finally {
      println(mod)
      typechecker.getErrors.foreach(println)
    }
    mod

  test("no body, no binding") {
    implicit val typechecker = new BaseIRTypechecker {}
    module(Relation("R", Seq(Param("p", TAny)), Seq()))
  }

  test("unbound param") {
    implicit val typechecker = new BaseIRTypechecker { }
    assertThrows[Failed](
      module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq()))))
    )
  }

  test("bound param") {
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(Var("p"), IntNum(1))
    )))))
    module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(IntNum(1), Var("p"))
    )))))
  }

  test("unbound param 2") {
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    assertThrows[Failed](
      module(Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Eq(Var("p1"), IntNum(1))
      )))))
    )
  }

  test("bound param 2") {
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
      Eq(Var("p1"), IntNum(1)),
      Eq(Var("p1"), Var("p2"))
    )))))
  }

  test("unbound variable in neq test") {
    assertThrows[Failed] {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(Relation("R", Seq(), Seq(Body(Seq(
        Neq(Var("x"), Var("y"))
      )))))
    }
    assertThrows[Failed]{
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(Relation("R", Seq(), Seq(Body(Seq(
        Neq(IntNum(0), Var("y"))
      )))))
    }
    assertThrows[Failed]{
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(Relation("R", Seq(), Seq(Body(Seq(
        Neq(Var("x"), IntNum(0))
      )))))
    }
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(), Seq(Body(Seq(
      Neq(IntNum(0), IntNum(0))
    )))))
  }

  test("call binds arguments") {
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Call("T", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
  }

  test("neg call requires bound arguments") {
    assertThrows[Failed] {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          NegCall("T", Seq(Var("p1"), Var("p2")))
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[Failed] {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          NegCall("T", Seq(Var("p1"), IntNum(2)))
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }
  }

  test("not inverts variable closing") {
    assertThrows[Failed] {
      implicit val typechecker = new BaseIRTypechecker with not.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Not(Call("T", Seq(Var("p1"), Var("p2"))))
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    // double negation
    implicit val typechecker = new BaseIRTypechecker with not.Typechecker {}
    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(Not(Call("T", Seq(Var("p1"), Var("p2")))))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )

    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(NegCall("T", Seq(Var("p1"), Var("p2"))))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
  }

  test("demand binds like a call") {
    implicit val typechecker = new BaseIRTypechecker with demand.Typechecker {}
    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Demand(Seq(Var("p1"), Var("p2")))
      ))))
    )

    assertThrows[Failed] {
      implicit val typechecker = new BaseIRTypechecker with demand.Typechecker with not.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Not(Demand(Seq(Var("p1"), Var("p2"))))
        ))))
      )
    }
  }

  // TODO: test extensional calls