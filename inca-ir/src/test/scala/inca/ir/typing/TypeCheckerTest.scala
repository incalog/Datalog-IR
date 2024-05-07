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
import inca.ir.term2Arg

class TypeCheckerTest extends AnyFunSuiteLike:

  def module(relations: ModuleEntry*)(using typechecker: () => BaseIRTypechecker): Module =
    val mod = Module("M", BaseIR.language, relations)
    val checker = typechecker()
    try checker.checkProgram(Seq(mod))
    finally {
      println(mod)
      checker.getErrors.foreach(println)
    }
    mod

  test("no body, no binding") {
    implicit val typechecker = () => new BaseIRTypechecker {}
    module(Relation("R", Seq(Param("p", TAny)), Seq()))
  }

  test("unbound param") {
    implicit val typechecker = () => new BaseIRTypechecker { }
    assertThrows[TypeErrorException](
      module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq()))))
    )
  }

  test("bound param") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
      Eq(Var("p"), IntNum(1))
    )))))
    module(Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
      Eq(IntNum(1), Var("p"))
    )))))
  }

  test("unbound param 2") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException](
      module(Relation("R", Seq(Param("p1", TInt), Param("p2", TAny)), Seq(Body(Seq(
        Eq(Var("p1"), IntNum(1))
      )))))
    )
  }

  test("bound param 2") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    module(Relation("R", Seq(Param("p1", TInt), Param("p2", TInt)), Seq(Body(Seq(
      Eq(Var("p1"), IntNum(1)),
      Eq(Var("p1"), Var("p2"))
    )))))
  }

  test("unbound variable in neq test") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException] {
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(Var("x"), Var("y"), true)
      )))))
    }
    assertThrows[TypeErrorException]{
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(IntNum(0), Var("y"), true)
      )))))
    }
    assertThrows[TypeErrorException]{
      module(Relation("R", Seq(), Seq(Body(Seq(
        Eq(Var("x"), IntNum(0), true)
      )))))
    }
    module(Relation("R", Seq(), Seq(Body(Seq(
      Eq(IntNum(0), IntNum(0), true)
    )))))
  }

  test("call binds arguments") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Call("T", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
  }

  test("neg call cannot bind arguments") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    {
      module(
        Relation("R", Seq(), Seq(Body(Seq(
          Call("T", Seq(Var("x").arg, Var("y").arg), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[TypeErrorException] {
      module(
        Relation("R", Seq(), Seq(Body(Seq(
          Call("T", Seq(Var("x").arg, Var("y").arg), true),
          Eq(Var("x"), Var("y"), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[TypeErrorException] {
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Call("T", Seq(Var("p1").arg, Var("p2").arg), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    assertThrows[TypeErrorException] {
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Call("T", Seq(Var("p1").arg, IntNum(2).arg), true)
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }
  }

  test("not inverts variable closing") {
    implicit val typechecker = () => new BaseIRTypechecker with not.Typechecker {}
    assertThrows[TypeErrorException] {
      module(
        Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
          Not(Call("T", Seq(Var("p1"), Var("p2"))))
        )))),
        Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
      )
    }

    // double negation
    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(Not(Call("T", Seq(Var("p1"), Var("p2")))))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )

    module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(Call("T", Seq(Var("p1").arg, Var("p2").arg), true))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
  }

  test("demand params") {
    implicit val typechecker = () => new BaseIRTypechecker with demand.Typechecker {}
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
      module(
        Relation("Q", Seq(Param("x", TAny), Param("y", TAny)), Seq(Body(Seq(
          Call("R", Seq(Var("x"), Var("y")))
        )))),
        Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        ))))
      )
    }
  }

  test("dependency graph") {
    implicit val typechecker = () => new BaseIRTypechecker with demand.Typechecker {}
    module(
      Relation("P", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        Call("Q", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("Q", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        Call("R", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        Call("P", Seq(Var("p1"), Var("p2")))
      ))))
    )

    assertThrows[TypeErrorException] {
      module(
        Relation("P", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
          Call("Q", Seq(Var("p1"), Var("p2")))
        )))),
        Relation("Q", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
          NegCall("R", Seq(Var("p1"), Var("p2")))
        )))),
        Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
          Call("P", Seq(Var("p1"), Var("p2")))
        ))))
      )
    }

    module(
      Relation("P", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        Call("Q", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("Q", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        NegCall("R", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
        NegCall("P", Seq(Var("p1"), Var("p2")))
      ))))
    )

    assertThrows[TypeErrorException] {
      module(
        Relation("P", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
          NegCall("Q", Seq(Var("p1"), Var("p2")))
        )))),
        Relation("Q", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
          NegCall("R", Seq(Var("p1"), Var("p2")))
        )))),
        Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
          NegCall("P", Seq(Var("p1"), Var("p2")))
        ))))
      )
    }
  }

  test("import type success") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    module(
      RelationImport("Q", Seq(TInt, TInt)),
      Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Call("Q", Seq(Var("x"), Var("y")))))))
    )
  }

  test("import type fail") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException] {
      module(
        RelationImport("Q", Seq(TInt, TInt)),
        Relation("R", Seq(Param("x", TNothing), Param("y", TNothing)), Seq(Body(Seq(
          Call("Q", Seq(Var("x"), Var("y")))))))
      )
    }
  }

  test("export type success") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    module(
      RelationImport("Q", Seq(TNothing, TNothing)),
      Relation("R", Seq(Param("x", TNothing), Param("y", TNothing)), Seq(Body(Seq(
        Call("Q", Seq(Var("x"), Var("y"))))))),
      RelationExport("R", Seq(TNothing, TNothing))
    )
  }

  test("export type fail") {
    implicit val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException] {
      module(
        RelationImport("Q", Seq(TNothing, TNothing)),
        Relation("R", Seq(Param("x", TNothing), Param("y", TNothing)), Seq(Body(Seq(
          Call("Q", Seq(Var("x"), Var("y"))))))),
        RelationExport("R", Seq(TInt, TInt))
      )
    }
  }
  // TODO: test extensional call