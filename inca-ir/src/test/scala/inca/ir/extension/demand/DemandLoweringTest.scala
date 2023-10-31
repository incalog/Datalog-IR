package inca.ir.extension.demand

import inca.ir.*
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.not.Not
import inca.ir.extension.{arithmetic, demand, not}
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker, TypeErrorException, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike

class DemandLoweringTest extends AnyFunSuiteLike:
  def module(relations: Relation*): Module =
    val typecheckerBefore = new IRTypechecker
    val typecheckerAfter = new IRTypechecker
    val lowering = new Lowering {}

    val mod = Module("M", BaseIR.language, relations)
    var printedMod = false
    var lowered: Module = null
    try {
      typecheckerBefore.typecheck(mod)
      println(mod)
      printedMod = true
      lowered = lowering.visitProgram(Seq(mod)).head
      typecheckerAfter.typecheck(lowered)
      lowered
    } finally {
      if (!printedMod)
        println(mod)
      println(lowered)
      val errorsBefore = typecheckerBefore.getErrors
      val errorsAfter = typecheckerAfter.getErrors
      if (errorsBefore.nonEmpty) {
        println("Type errors in original code:")
        errorsBefore.foreach(println)
      }
      if (errorsAfter.nonEmpty) {
        println("Type errors in lowered code:")
        errorsAfter.foreach(println)
      }
    }

  test("no body, no binding") {
    val m = module(Relation("R", Seq(Param("p", TAny)), Seq()))
    assert(m.relations.size == 1)
  }

  test("bound param") {
    val m1 = module(Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
      Eq(Var("p"), IntNum(1))
    )))))
    assert(m1.relations.size == 1)

    val m2 = module(Relation("R", Seq(Param("p", TInt)), Seq(Body(Seq(
      Eq(IntNum(1), Var("p"))
    )))))
    assert(m2.relations.size == 1)
  }

  test("bound param 2") {
    val m = module(Relation("R", Seq(Param("p1", TInt), Param("p2", TInt)), Seq(Body(Seq(
      Eq(Var("p1"), IntNum(1)),
      Eq(Var("p1"), Var("p2"))
    )))))
    assert(m.relations.size == 1)
  }

  test("unbound variable in neq test") {
    val m = module(Relation("R", Seq(), Seq(Body(Seq(
      Neq(IntNum(0), IntNum(0))
    )))))
    assert(m.relations.size == 1)
  }

  test("call binds arguments") {
    val m = module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Call("T", Seq(Var("p1"), Var("p2")))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
    assert(m.relations.size == 2)
  }

  test("not inverts variable closing") {
    // double negation
    val m1 = module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(Not(Call("T", Seq(Var("p1"), Var("p2")))))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
    assert(m1.relations.size == 2)

    val m2 = module(
      Relation("R", Seq(Param("p1", TAny), Param("p2", TAny)), Seq(Body(Seq(
        Not(NegCall("T", Seq(Var("p1"), Var("p2"))))
      )))),
      Relation("T", Seq(Param("x1", TAny), Param("x2", TAny)), Seq())
    )
    assert(m2.relations.size == 2)
  }

  test("demand type annotation") {
    val dem = module(
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (dem.relations("R").bodies.head.atoms.head)
  }

  test("demand propagates") {
    val m1 = module(
      Relation("Q", Seq(Param("x", TDemand(TAny)), Param("y", TDemand(TAny))), Seq(Body(Seq(
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m1.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"), Var("y"))))
      (m1.relations("Q").bodies.head.atoms.head)

    val m2 = module(
      Relation("Q", Seq(Param("x", TDemand(TAny)), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m2.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"))))
      (m2.relations("Q").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"))))
      (m2.relations(demandRelationName("R")).bodies.head.atoms.head)
  }

  test("demand propagates through locals") {
    val m1 = module(
      Relation("Q", Seq(Param("x", TDemand(TAny)), Param("y", TDemand(TAny))), Seq(Body(Seq(
        Eq(Var("x1"), Var("x")), Eq(Var("y1"), Var("y")),
        Call("R", Seq(Var("x1"), Var("y1")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m1.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"), Var("y"))))
      (m1.relations("Q").bodies.head.atoms.head)

    val m2 = module(
      Relation("Q", Seq(Param("x", TDemand(TAny)), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Eq(Var("x1"), Var("x")), Eq(Var("y1"), Var("y")),
        Call("R", Seq(Var("x1"), Var("y1")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m2.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"))))
      (m2.relations("Q").bodies.head.atoms.head)

  }

  test("demand propagates transitively") {
    val m1 = module(
      Relation("P", Seq(Param("a", TDemand(TAny))), Seq(Body(Seq(
        Call("Q", Seq(Var("a"), Var("a")))
      )))),
      Relation("Q", Seq(Param("x", TDemand(TAny)), Param("y", TDemand(TAny))), Seq(Body(Seq(
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m1.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"), Var("y"))))
      (m1.relations("Q").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("P"), Seq(Var("a"))))
      (m1.relations("P").bodies.head.atoms.head)


    val m2 = module(
      Relation("P", Seq(Param("a", TInt)), Seq(Body(Seq(
        Eq(Var("a"), IntNum(1)),
        Call("Q", Seq(Var("a"), Var("a")))
      )))),
      Relation("Q", Seq(Param("x", TDemand(TInt)), Param("y", TDemand(TInt))), Seq(Body(Seq(
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m2.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"), Var("y"))))
      (m2.relations("Q").bodies.head.atoms.head)
    assertResult
      (Call("Q", Seq(Var("a"), Var("a"))))
      (m2.relations("P").bodies.head.atoms(1))
  }

  test("demand propagates selectively") {
    val m1 = module(
      Relation("Q", Seq(Param("x", TDemand(TInt)), Param("y", TDemand(TInt))), Seq(
        Body(Seq(Call("R", Seq(Var("x"), IntNum(1))), Eq(Var("y"), IntNum(1)))),
        Body(Seq(Call("R", Seq(Var("y"), IntNum(1))), Eq(Var("x"), IntNum(1)))),
      )),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m1.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"), Var("y"))))
      (m1.relations("Q").bodies.head.atoms.head)

    assertThrows[TypeErrorException](module( // y needs to be demanded
      Relation("Q", Seq(Param("x", TDemand(TInt)), Param("y", TInt)), Seq(
        Body(Seq(Call("R", Seq(Var("x"), IntNum(1))), Eq(Var("y"), IntNum(1)))),
        Body(Seq(Call("R", Seq(Var("y"), IntNum(1))), Eq(Var("x"), IntNum(1)))),
      )),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    ))

    assertThrows[TypeErrorException](module( // x needs to be demanded
      Relation("Q", Seq(Param("x", TInt), Param("y", TDemand(TInt))), Seq(
        Body(Seq(Call("R", Seq(Var("x"), IntNum(1))), Eq(Var("y"), IntNum(1)))),
        Body(Seq(Call("R", Seq(Var("y"), IntNum(1))), Eq(Var("x"), IntNum(1)))),
      )),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    ))


    val m2 = module(
      Relation("Q", Seq(Param("x", TDemand(TInt)), Param("y", TInt)), Seq(
        Body(Seq(Call("R", Seq(Var("x"), IntNum(1))), Eq(Var("y"), IntNum(1)))),
        Body(Seq(Call("R", Seq(IntNum(1), Var("x"))), Eq(Var("y"), IntNum(1)))),
      )),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )
    assertResult
      (Call(demandRelationName("R"), Seq(Var("p1"), Var("p2"))))
      (m2.relations("R").bodies.head.atoms.head)
    assertResult
      (Call(demandRelationName("Q"), Seq(Var("x"))))
      (m2.relations("Q").bodies.head.atoms.head)
  }

  test("demand propagates fork/join") {
    val dem = module(
      Relation("P", Seq(Param("x", TDemand(TInt)), Param("y", TDemand(TInt))), Seq(
        Body(Seq(Call("Q1", Seq(Var("x"), Var("y"))))),
        Body(Seq(Call("Q2", Seq(Var("x"), Var("y"))))),
      )),
      Relation("Q1", Seq(Param("x", TDemand(TInt)), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("Q2", Seq(Param("x", TInt), Param("y", TDemand(TInt))), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )

    val dem2 = module(
      Relation("P", Seq(Param("x", TDemand(TInt)), Param("y", TInt)), Seq(
        Body(Seq(Call("Q1", Seq(Var("x"), Var("y"))))),
        Body(Seq(Call("Q2", Seq(Var("y"), Var("x"))))),
      )),
      Relation("Q1", Seq(Param("x", TDemand(TInt)), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("Q2", Seq(Param("x", TInt), Param("y", TDemand(TInt))), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )

    module(
      Relation("P", Seq(Param("x", TDemand(TInt)), Param("y", TDemand(TInt))), Seq(
        Body(Seq(Call("Q1", Seq(Var("x"), Var("y"))))),
        Body(Seq(Call("Q2", Seq(Var("y"), Var("x"))))),
      )),
      Relation("Q1", Seq(Param("x", TDemand(TInt)), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("Q2", Seq(Param("x", TInt), Param("y", TDemand(TInt))), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Call("R", Seq(Var("x"), Var("y")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq(
      ))))
    )
  }