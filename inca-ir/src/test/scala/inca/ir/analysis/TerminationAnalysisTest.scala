package inca.ir.analysis

import inca.ir.analysis.base.values.{FiniteAbstractRelation, Value}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator.MinInt
import inca.ir.extension.arithmetic.{Add, ArithmeticAggregationOperator, GT, IntNum, LE, LT, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.string.analysis.interpreter.FiniteStringV
import inca.ir.extension.string.{StringConcat, StringLength, StringLit, Substring, TString, IR as stringIR}
import inca.ir.hints.MainHint
import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Call, Cast, Eq, ExtensionalCall, ExtensionalRelation, Module, Param, RefByName, Relation, TNothing, Var, WildcardArg, string2name, term2Arg, termList2ArgList}
import org.scalatest.funsuite.AnyFunSuiteLike
import sturdy.values.Topped

class TerminationAnalysisTest extends AnyFunSuiteLike:

  def interp(mod: Module, edb: Map[String, FiniteAbstractRelation] = Map()): Map[String, FiniteAbstractRelation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val abstractInterp = IRTerminationAbstractInterpreter(interRelational = true)
    edb.foreach(abstractInterp.insertEDB)
    abstractInterp.evalProgram(Seq(mod))
    abstractInterp.getIDB

  test("Generate numbers") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("main", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("main", Seq(Var("m"))),
          GT(Var("m"), IntNum(1)),
          Eq(Var("n"), Sub(Var("m"), IntNum(1)))
        )),
        Body(Seq(
          Eq(Var("n"), IntNum(1000)),
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val mainRel = res("main")
    // n in [1, 1000]
    assert(mainRel.rows.head.isFinite)
    assert(mainRel.finite.isActual && mainRel.finite.get)
    //println(res)
  }

  test("Generate numbers - 2") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("A", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(5)),
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(100)),
        )),
      )),
      Relation("B", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(0)),
        )),
        Body(Seq(
          Call("A", Seq(IntNum(6))),
          Call("B", Seq(Var("y"))),
          Eq(Var("x"), Add(Var("y"), IntNum(1))),
        )),
      )),
      Relation("main", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("B", Seq(Var("n"))),
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    println(res)
    val mainRel = res("main")
    assert(mainRel.rows.head.isFinite)
    assert(mainRel.finite.isActual && mainRel.finite.get)
  }

  test("Min Aggregation") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("input1", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(Eq(Var("n"), IntNum(1)))),
        Body(Seq(Eq(Var("n"), IntNum(2)))),
        Body(Seq(Eq(Var("n"), IntNum(3))))
      )),
      Relation("input2", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(Eq(Var("n"), IntNum(3)))),
        Body(Seq(Eq(Var("n"), IntNum(4)))),
        Body(Seq(Eq(Var("n"), IntNum(5))))
      )),
      Relation("main", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("input1", Seq(Var("m"))),
          Aggregate("input2", Seq(AggregateColumnArg(Var("n"))), MinInt),
          LE(Var("n"), Var("m"))
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val mainRel = res("main")
    // n is [3, 3]
    assert(mainRel.rows.head.isFinite)
    assert(mainRel.finite.isActual && mainRel.finite.get)
    // input2 -> [3, 5] since we are calculating the min on the whole joined relation result and not after each body.
    //println(res)
  }

  test("Infinite Int") {
    val mod = Module("MethodLookup", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("DirectSuperclass", Seq(
        Param("type", TString),
        Param("supertype", TString)
      )),

      ExtensionalRelation("MethodImplemented", Seq(
        Param("type", TString),
        Param("method", TString)
      )),

      Relation("_MethodLookup_WithLen", Seq(
        Param("type", TString),
        Param("method", TString),
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("method"))),
          Eq(Var("n"), IntNum(0))
        )),
        Body(Seq(
          ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("supertype"))),
          Call("_MethodLookup_WithLen", Seq(Var("supertype"), Var("method"), Var("n0"))),
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("_")), true),
          Eq(Var("n"), Add(Var("n0"), IntNum(1)))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod, Map(
      "DirectSuperclass" -> FiniteAbstractRelation.finiteNonEmpty(
        Seq("type", "supertype"),
        Seq(FiniteStringV.edb(), FiniteStringV.edb())
      ),
      "MethodImplemented" -> FiniteAbstractRelation.finiteNonEmpty(
        Seq("type", "method"),
        Seq(FiniteStringV.edb(), FiniteStringV.edb())
      )
    ))
    val mainRel = res("_MethodLookup_WithLen")
    assertResult(mainRel.finite.isTop)
    assertResult(mainRel.rows(0).isFinite)
    assertResult(mainRel.rows(1).isFinite)
    assertResult(!mainRel.rows(2).isFinite)
  }

  test("Finite Int (Bounded by count)") {
    val mod = Module("MethodLookup", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("DirectSuperclass", Seq(
        Param("type", TString),
        Param("supertype", TString)
      )),

      ExtensionalRelation("MethodImplemented", Seq(
        Param("type", TString),
        Param("method", TString)
      )),

      Relation("TransitiveSuperclasses",
        Seq(
          Param("type", TString),
          Param("supertype", TString)
        ),
        Seq(
          Body(Seq(
            ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("supertype"))),
          )),
          Body(Seq(
            ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("stype"))),
            Call("TransitiveSuperclasses", Seq(Var("stype"), Var("supertype"))),
          ))
        )
      ),

      Relation("_MethodLookup_WithLen", Seq(
        Param("type", TString),
        Param("method", TString),
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("method"))),
          Eq(Var("n"), IntNum(0))
        )),
        Body(Seq(
          ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("supertype"))),
          Call("_MethodLookup_WithLen", Seq(Var("supertype"), Var("method"), Var("n0"))),
          Aggregate("TransitiveSuperclasses", Seq(Var("type").arg, AggregateColumnArg(Var("c"))), ArithmeticAggregationOperator.Count),
          LE(Var("n0"), Var("c")),
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("_")), true),
          Eq(Var("n"), Add(Var("n0"), IntNum(1))),
        ))
      )).addHint(MainHint)
    ))

    println(mod)

    val res = interp(mod, Map(
      "DirectSuperclass" -> FiniteAbstractRelation.finiteNonEmpty(
        Seq("type", "supertype"),
        Seq(FiniteStringV.edb(), FiniteStringV.edb())
      ),
      "MethodImplemented" -> FiniteAbstractRelation.finiteNonEmpty(
        Seq("type", "method"),
        Seq(FiniteStringV.edb(), FiniteStringV.edb())
      )
    ))

    val helperRel = res("TransitiveSuperclasses")
    assertResult(helperRel.finite.isActual && helperRel.finite.get)
    assertResult(helperRel.rows(0).isFinite)
    assertResult(helperRel.rows(1).isFinite)

    val mainRel = res("_MethodLookup_WithLen")
    assertResult(mainRel.finite.isActual && mainRel.finite.get)
    assertResult(mainRel.rows(0).isFinite)
    assertResult(mainRel.rows(1).isFinite)
    assertResult(!mainRel.rows(2).isFinite)
  }

  test("Path counting length") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TString),
        Param("y", TString)
      ), Seq(
        Body(Seq(Eq(Var("x"), StringLit("a")), Eq(Var("y"), StringLit("b")))),
        Body(Seq(Eq(Var("x"), StringLit("b")), Eq(Var("y"), StringLit("c")))),
        Body(Seq(Eq(Var("x"), StringLit("c")), Eq(Var("y"), StringLit("d")))),
        Body(Seq(Eq(Var("x"), StringLit("a")), Eq(Var("y"), StringLit("e")))),
        Body(Seq(Eq(Var("x"), StringLit("e")), Eq(Var("y"), StringLit("d"))))
      )),

      Relation("path", Seq(
        Param("x", TString),
        Param("y", TString),
        Param("len", TInt)
      ), Seq(
        // path(x, y, 1) :- edge(x, y).
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y"))),
          Eq(Var("len"), IntNum(1))
        )),

        // path(x, z, l+1) :- edge(x, y), path(y, z, l).
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y"))),
          Call("path", Seq(Var("y"), Var("z"), Var("l"))),
          Eq(Var("len"), Add(Var("l"), IntNum(1)))
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val edgeRel = res("edge")
    assertResult(edgeRel.finite.isActual && edgeRel.finite.get)
    assertResult(edgeRel.rows(0).isFinite)
    assertResult(edgeRel.rows(1).isFinite)

    val pathRel = res("path")
    assertResult(pathRel.finite.isTop)
    assertResult(pathRel.rows(0).isFinite)
    assertResult(pathRel.rows(1).isFinite)
    assertResult(!pathRel.rows(2).isFinite)
  }

  test("Concat String") {
    val mod = Module("ConcatTest", BaseIR.language + arithIR, Seq(
      Relation("main", Seq(
        Param("n", TString),
      ), Seq(
        Body(Seq(
          Call("main", Seq(Var("m"))),
          Eq(Var("o"), StringConcat(StringLit("a"), Var("m"))),
          // Defensive programming to prevent infinite loop
          Eq(Var("n"), Substring(Var("o"), IntNum(0), IntNum(100)))
        )),
        Body(Seq(
          Eq(Var("n"), StringLit("b"))
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    println(res)
    val mainRel = res("main")
    // n in [1, 1000]
    assert(mainRel.rows.head.isFinite)
    assert(mainRel.finite.isActual && mainRel.finite.get)
    //println(res)
  }