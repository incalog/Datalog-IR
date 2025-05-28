package inca.ir.analysis

import inca.ir.analysis.base.values.{FiniteAbstractRelation, Value}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator.MinInt
import inca.ir.extension.arithmetic.{Add, GT, IntNum, LE, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.string.analysis.interpreter.FiniteStringV
import inca.ir.extension.string.{StringLit, TString, IR as stringIR}
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
    assert(mainRel.rows.head.isFinite)
    assert(mainRel.finite.isActual && mainRel.finite.get)
    //println(res)
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
    assert(mainRel.rows.head.isFinite) // should be constant 3
    assert(mainRel.finite.isActual && mainRel.finite.get)
    // input2 -> [3, 5] since we are calculating the min on the whole joined relation result and not after each body.
    //println(res)
  }