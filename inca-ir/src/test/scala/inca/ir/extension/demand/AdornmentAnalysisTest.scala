package inca.ir.extension.demand

import inca.ir.*
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.demand.Demand
import inca.ir.extension.{arithmetic, demand, not}
import inca.ir.extension.not.Not
import inca.ir.typing.{BaseIRTypechecker, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.MultiDict

class AdornmentAnalysisTest extends AnyFunSuiteLike:

  def module(relations: Relation*): MultiDict[String, String] =
    val typechecker = new BaseIRTypechecker with demand.Typechecker with not.Typechecker with arithmetic.Typechecker {}
    val adornment = new AdornmentAnalysis with demand.Visitor with not.Visitor with arithmetic.Visitor {}

    val mod = Module("M", BaseIR.language, relations)
    try {
      typechecker.typecheck(mod)
      adornment.visit(mod)
      adornment.demandedParams
    }
    finally {
      println(mod)
      typechecker.getErrors.foreach(println)
    }

  test("no body, no binding") {
    val dem = module(Relation("R", Seq(Param("p", TAny)), Seq()))
    assert(dem.isEmpty)
  }

  test("bound param") {
    val dem1 = module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(Var("p"), IntNum(1))
    )))))
    assert(dem1.isEmpty)

    val dem2 = module(Relation("R", Seq(Param("p", TAny)), Seq(Body(Seq(
      Eq(IntNum(1), Var("p"))
    )))))
    assert(dem2.isEmpty)
  }

  test("bound param 2") {
    val dem = module(Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
      Eq(Var("p1"), IntNum(1)),
      Eq(Var("p1"), Var("p2"))
    )))))
    assert(dem.isEmpty)
  }

  test("unbound variable in neq test") {
    val dem = module(Relation("R", Seq(), Seq(Body(Seq(
      Neq(IntNum(0), IntNum(0))
    )))))
    assert(dem.isEmpty)
  }

  test("call binds arguments") {
    val dem = module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Call("T", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
    assert(dem.isEmpty)
  }

  test("not inverts variable closing") {
    // double negation
    val dem = module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(Not(Call("T", Seq(Var("p1"), Var("p2")))))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
    assert(dem.isEmpty)

    val dem2 = module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(NegCall("T", Seq(Var("p1"), Var("p2"))))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
    assert(dem2.isEmpty)
  }

  test("demand binds like a call") {
    val dem = module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Demand(Seq(Var("p1"), Var("p2")))
      ))))
    )
    assert(dem.containsEntry(("R", "p1")))
    assert(dem.containsEntry(("R", "p2")))
  }
