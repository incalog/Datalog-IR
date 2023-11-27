package inca.ir.typing

import inca.ir.*
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.not
import inca.ir.extension.not.Not
import inca.ir.extension.demand
import inca.ir.extension.demand.TDemand
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
    assertThrows[TypeErrorException](
      module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq()))))
    )
  }

  test("bound param") {
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
      Eq(Var("p"), IntNum(1))
    )))))
    module(Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
      Eq(IntNum(1), Var("p"))
    )))))
  }

  test("unbound param 2") {
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException](
      module(Relation("R", Seq(Param("p1", TInt), Param("p2", TAny)), Seq(Body(Seq(
        Eq(Var("p1"), IntNum(1))
      )))))
    )
  }

  test("bound param 2") {
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(Param("p1", TInt), Param("p2", TInt)), Seq(Body(Seq(
      Eq(Var("p1"), IntNum(1)),
      Eq(Var("p1"), Var("p2"))
    )))))
  }

  test("unbound variable in neq test") {
    assertThrows[TypeErrorException] {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(Var("x"), Var("y"), true)
      )))))
    }
    assertThrows[TypeErrorException]{
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(IntNum(0), Var("y"), true)
      )))))
    }
    assertThrows[TypeErrorException]{
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(Var("x"), IntNum(0), true)
      )))))
    }
    implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(), Seq(Body(Seq(
      Eq(IntNum(0), IntNum(0), true)
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

  test("neg call cannot bind arguments") {
    {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(), Seq(Body(Seq(
          Call("T", Seq(Var("x"), Var("y")), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[TypeErrorException] {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(), Seq(Body(Seq(
          Call("T", Seq(Var("x"), Var("y")), true),
          Eq(Var("x"), Var("y"), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[TypeErrorException] {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Call("T", Seq(Var("p1"), Var("p2")), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[TypeErrorException] {
      implicit val typechecker = new BaseIRTypechecker with arithmetic.Typechecker {}
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Call("T", Seq(Var("p1"), IntNum(2)), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }
  }

  test("not inverts variable closing") {
    assertThrows[TypeErrorException] {
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
        Not(Call("T", Seq(Var("p1"), Var("p2")), true))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
  }

  test("demand params") {
    implicit val typechecker = new BaseIRTypechecker with demand.Typechecker {}
    module(
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
      ))))
    )

    module(
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TAny)), Seq(Body(Seq(
        Eq(Var("p1"), Var("p2"))
      ))))
    )

    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        Eq(Var("p1"), Var("p2"))
      ))))
    )

    module(
      Relation("Q", Seq(Param("x", TDemand(TAny)), Param("y", TDemand(TAny))), Seq(Body(Seq(
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
      ))))
    )

    assertThrows[TypeErrorException] {
      implicit val typechecker = new BaseIRTypechecker with demand.Typechecker {}
      module(
        Relation("Q", Seq(Param("x", TAny), Param("y", TAny)), Seq(Body(Seq(
          Call("R", Seq(Var("x"), Var("y")))
        )))),
        Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        ))))
      )
    }
  }

  // TODO: test extensional calls