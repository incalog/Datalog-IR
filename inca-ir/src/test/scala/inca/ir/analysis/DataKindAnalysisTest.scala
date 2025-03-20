package inca.ir.analysis

import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
import inca.ir.extension.arithmetic.{Add, IntNum, LT, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.bool.analysis.interpreter.ConstantBoolV
import inca.ir.extension.bool.{AtomAsBool, BoolFalse, BoolTrue, TBoolean, IR as boolIR}
import inca.ir.extension.data.analysis.interpreter.DataKindV
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.map.analysis.interpreter.BoundedMapV
import inca.ir.extension.map.{MapComprehension, MapContains, MapFrom, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.not.Not
import inca.ir.extension.set.analysis.interpreter.BoundedSetV
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

import scala.annotation.tailrec

class DataKindAnalysisTest extends AnyFunSuiteLike:

  @tailrec
  private def verifyResult(result: Value, expectedDataKinds: Set[String]): Unit =
    result match
      case BoundedSetV.Empty => assertResult(expectedDataKinds)(Set())
      case BoundedSetV.NonEmpty(bound) => verifyResult(bound, expectedDataKinds)
      case DataKindV(caseDefs) => assertResult(expectedDataKinds)(caseDefs.map(_.name.name))
      case _ => assert(false)

  def interp(mod: Module, edb: Map[String, AbstractRelation] = Map()): Map[String, AbstractRelation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val abstractInterp = IRDataKindAbstractInterpreter(interRelational = true)
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

  /* Set */

  test("Set - Literal") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR + setIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("main", Seq(
        Param("out", TSet(TData("TList")))
      ), Seq(
        Body(Seq(
          Eq(Var("out"), SetLit(Seq(
            Construct("TNil", Seq()),
            Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))
          )))
        ))
      )).addHint(MainHint)
    ))

    val constRes = interp(mod)
    val mainRel = constRes("main")
    assert(mainRel.cols == Seq("out"))
    verifyResult(mainRel.rows.head, Set("TNil", "TCons"))
    assertResult(Topped.Actual(false))(mainRel.empty)
  }

  test("Set - Union") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR + setIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("main", Seq(
        Param("out", TSet(TData("TList")))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(
            Construct("TNil", Seq()),
          ))),
          Eq(Var("y"), SetLit(Seq(
            Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))
          ))),
          Eq(Var("out"), SetUnion(Var("x"), Var("y")))
        ))
      )).addHint(MainHint)
    ))

    val constRes = interp(mod)
    val mainRel = constRes("main")
    assert(mainRel.cols == Seq("out"))
    verifyResult(mainRel.rows.head, Set("TNil", "TCons"))
    assertResult(Topped.Actual(false))(mainRel.empty)
  }

  test("Set - Intersect") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR + setIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("main", Seq(
        Param("out", TSet(TData("TList")))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(
            Construct("TNil", Seq()),
          ))),
          Eq(Var("y"), SetLit(Seq(
            Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))
          ))),
          Eq(Var("out"), SetIntersection(Var("x"), Var("y")))
        ))
      )).addHint(MainHint)
    ))

    // We expect the empty set, since there is no valid Data kind in the intersection

    val constRes = interp(mod)
    val mainRel = constRes("main")
    assert(mainRel.cols == Seq("out"))
    verifyResult(mainRel.rows.head, Set())
    assertResult(Topped.Actual(false))(mainRel.empty)
  }

  test("Set - Member (binding)") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR + setIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("main", Seq(
        Param("out", TData("TList"))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(
            Construct("TNil", Seq()),
            Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))
          ))),
          SetMember(Var("out"), Var("x"))
        ))
      )).addHint(MainHint)
    ))

    val constRes = interp(mod)
    val mainRel = constRes("main")
    assert(mainRel.cols == Seq("out"))
    verifyResult(mainRel.rows.head, Set("TNil", "TCons"))
    assertResult(Topped.Top)(mainRel.empty)
  }

  /* Map */

  test("Map - Empty") {
    val mod = Module("Test1", BaseIR.language + mapIR, Seq(
      Relation(
        "main",
        Seq(
          Param("m", TMap(TNothing, TNothing))
        ),
        Seq(Body(Seq(
          Eq(Var("m"), MapLit.empty)
        )))
      )
    ))
    val constRes = interp(mod)
    val mainRel = constRes("main")
    assert(mainRel.cols == Seq("m"))
    assert(mainRel.rows == Seq(BoundedMapV.empty))
    assertResult(Topped.Actual(false))(mainRel.empty)
  }

  test("Map - Literal") {
    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation(
        "main",
        Seq(Param("m", TMap(TString, TData("TList")))),
        Seq(Body(Seq(
          Eq(Var("m"), MapLit.from(
            (StringLit("A"), Construct("TNil", Seq())),
            (StringLit("B"), Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))))
          )
        )))
      )
    ))
    val constRes = interp(mod)
    val mainRel = constRes("main")
    assert(mainRel.cols == Seq("m"))
    val map = mainRel.rows.head.asInstanceOf[BoundedMapV]
    assert(map.key == Value.Top)
    verifyResult(map.value, Set("TNil", "TCons"))
    assertResult(Topped.Actual(false))(mainRel.empty)
  }