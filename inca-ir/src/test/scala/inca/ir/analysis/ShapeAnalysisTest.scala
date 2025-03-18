package inca.ir.analysis

import inca.ir.analysis.base.values.{ConstantRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
import inca.ir.extension.arithmetic.{Add, IntNum, LT, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.bool.analysis.interpreter.ConstantBoolV
import inca.ir.extension.bool.{AtomAsBool, BoolFalse, BoolTrue, TBoolean, IR as boolIR}
import inca.ir.extension.data.analysis.interpreter.ConstantDataV
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.map.analysis.interpreter.ConstantMapV
import inca.ir.extension.map.{MapComprehension, MapContains, MapFrom, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.not.Not
import inca.ir.extension.set.analysis.interpreter.ConstantSetV
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet, IR as setIR}
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.extension.string.{StringLit, TString, IR as stringIR}
import inca.ir.extension.tuple.analysis.interpreter.ConstantTupleV
import inca.ir.extension.tuple.{Project, TTuple, TupleLit, IR as tupleIR}
import inca.ir.printer.IRDebugPrinter
import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Call, Eq, ExtensionalCall, ExtensionalRelation, MainHint, Module, Param, RefByName, Relation, TNothing, Var, WildcardArg, string2name, term2Arg, termList2ArgList}
import org.scalatest.funsuite.AnyFunSuiteLike
import sturdy.values.Topped

class ShapeAnalysisTest extends AnyFunSuiteLike:

  def interp(mod: Module, edb: Map[String, ConstantRelation] = Map()): Map[String, ConstantRelation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val abstractInterp = IRShapeAbstractInterpreter(interRelational = true)
    edb.foreach(abstractInterp.insertEDB)
    abstractInterp.evalProgram(Seq(mod))
    val res = abstractInterp.getIDB

    //val printer = new IRDebugPrinter {}
    //println(printer.prettyPrint(mod))

    res

  test("ADT - Construct") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("helper", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2))
        ))
      )),

      Relation("main", Seq(
        Param("x", TData("TList")),
      ), Seq(
        Body(Seq(
          Call("helper", Seq(Var("y"))),
          Eq(Var("x"), Construct("TCons", Seq(Var("y"), Construct("TNil", Seq())))),
        )),
      )).addHint(MainHint),
    ))

    val constRes = interp(mod)
    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows.head.toString == "{TCons(?,?)}")
    assertResult(Topped.Actual(false))(mainEdgeRelType.empty)
  }

  test("ADT - Deconstruct dispatch") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("helper", Seq(
        Param("x", TData("TList")),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), Construct("TNil", Seq()))
        )),
        Body(Seq(
          Eq(Var("x"), Construct("TCons", Seq(IntNum(5), Construct("TNil", Seq()))))
        )),
        Body(Seq(
          Eq(Var("x"), Construct("TCons", Seq(IntNum(8), Construct("TNil", Seq()))))
        ))
      )),

      Relation("main", Seq(
        Param("x", TInt),
        Param("y", TData("TList")),
      ), Seq(
        Body(Seq(
          Call("helper", Seq(Var("y"))),
          Deconstruct(Var("y"), "TCons", Seq(Var("x").arg, WildcardArg()), false)
        )),
        Body(Seq(
          Call("helper", Seq(Var("y"))),
          Deconstruct(Var("y"), "TNil", Seq(), false),
          Eq(Var("x"), IntNum(-1))
        )),
      )).addHint(MainHint),
    ))

    val constRes = interp(mod)
    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows.head.toString == "{TNil(),TCons(?,?)}")
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x", "y"))
    assert(mainEdgeRelType.rows.head == Value.Top)
    assert(mainEdgeRelType.rows.last.toString == "{TNil(),TCons(?,?)}")
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }