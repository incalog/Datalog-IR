//package inca.ir.analysis
//
//import inca.ir.analysis.base.values.{ConstantRelation, Value}
//import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
//import inca.ir.extension.arithmetic.{Add, IntNum, LT, Mul, Sub, TInt, IR as arithIR}
//import inca.ir.extension.bool.analysis.interpreter.ConstantBoolV
//import inca.ir.extension.bool.{AtomAsBool, BoolFalse, BoolTrue, TBoolean, IR as boolIR}
//import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
//import inca.ir.extension.demand.TDemand
//import inca.ir.extension.map.analysis.interpreter.ConstantMayMapV
//import inca.ir.extension.map.{MapComprehension, MapContains, MapFrom, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
//import inca.ir.extension.not.Not
//import inca.ir.extension.set.analysis.interpreter.ConstantMaySetV
//import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet, IR as setIR}
//import inca.ir.extension.string.analysis.interpreter.ConstantStringV
//import inca.ir.extension.string.{StringLit, TString, IR as stringIR}
//import inca.ir.extension.tuple.analysis.interpreter.ConstantTupleV
//import inca.ir.extension.tuple.{Project, TTuple, TupleLit, IR as tupleIR}
//import inca.ir.printer.IRDebugPrinter
//import inca.ir.typing.IRTypechecker
//import inca.ir.{BaseIR, Body, Call, Cast, Eq, ExtensionalCall, ExtensionalRelation, MainHint, Module, Param, RefByName, Relation, TNothing, Var, WildcardArg, string2name, term2Arg, termList2ArgList}
//import org.scalatest.funsuite.AnyFunSuiteLike
//import sturdy.values.Topped
//
//class ConstantMayAnalysisTest extends AnyFunSuiteLike:
//
//  def interp(mod: Module, edb: Map[String, ConstantRelation] = Map()): Map[String, ConstantRelation] =
//    val typechecker = new IRTypechecker
//    typechecker.checkProgram(Seq(mod))
//
//    val abstractInterp = IRConstantAbstractInterpreter(interRelational = true)
//    edb.foreach(abstractInterp.insertEDB)
//    abstractInterp.evalProgram(Seq(mod))
//    val res = abstractInterp.getIDB
//
//    //val printer = new IRDebugPrinter {}
//    //println(printer.prettyPrint(mod))
//
//    res
//
//  /* Set */
//
//  test("Set - Literal") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TSet(TInt))
//      ), Seq(
//        Body(Seq(
//          Eq(Var("out"), SetLit(Seq(IntNum(1), IntNum(2))))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assert(mainRel.rows.head == ConstantMaySetV(ConstantIntV(1), ConstantIntV(2)))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Set - Union") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TSet(TInt))
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2)))),
//          Eq(Var("y"), SetLit(Seq(IntNum(2), IntNum(3)))),
//          Eq(Var("out"), SetUnion(Var("x"), Var("y")))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assert(mainRel.rows.head == ConstantMaySetV(ConstantIntV(1), ConstantIntV(2), ConstantIntV(3)))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Set - Intersect") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TSet(TInt))
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2)))),
//          Eq(Var("y"), SetLit(Seq(IntNum(2), IntNum(3)))),
//          Eq(Var("out"), SetIntersection(Var("x"), Var("y")))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assert(mainRel.rows.head == ConstantMaySetV(ConstantIntV(2)))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Set - Member (binding)") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TInt)
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
//          SetMember(Var("out"), Var("x"))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assert(mainRel.rows.head == Value.Top)
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Set - Member (bound)") {
//    val mod = Module("Test1", BaseIR.language + boolIR + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TBoolean)
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
//          SetMember(IntNum(1), Var("x")), // we can not decide this here, since this is a `may-set`
//          Eq(BoolTrue, Var("out"))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assert(mainRel.rows.head == ConstantBoolV(true))
//    // top, since the containment check could succeed or fail
//    assertResult(Topped.Top)(mainRel.empty)
//  }
//
//  test("Set - Comprehension") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TSet(TInt))
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
//          Eq(Var("out"), SetComprehension(Var("x$i"), Seq(
//            SetMember(Var("x$i"), Var("x")),
//            LT(Var("x$i"), IntNum(3))
//          )))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assert(mainRel.rows.head == ConstantMaySetV.top)
//    assertResult(Topped.Top)(mainRel.empty)
//  }
//
//  test("Set - Member (Empty)") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TInt)
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), Cast(SetLit(Seq()), TSet(TInt))),
//          SetMember(Var("out"), Var("x"))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assertResult(Topped.Actual(true))(mainRel.empty)
//  }
//
//  test("Set - Comprehension (Empty)") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("main", Seq(
//        Param("out", TSet(TInt))
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
//          Eq(Var("out"), SetComprehension(Var("x$i"), Seq(
//            SetMember(Var("x$i"), Var("x")),
//            LT(Var("x$i"), IntNum(0))
//          )))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("out"))
//    assert(mainRel.rows.head == ConstantMaySetV.top)
//    assertResult(Topped.Top)(mainRel.empty)
//  }
//
//  test("Set - SetFrom") {
//    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
//      Relation("nums", Seq(
//        Param("x", TInt),
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), IntNum(1)),
//        )),
//        Body(Seq(
//          Eq(Var("x"), IntNum(2)),
//        ))
//      )),
//      Relation("main", Seq(
//        Param("x", TSet(TInt))
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetFrom(RefByName("nums")))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("x"))
//    assert(mainRel.rows.head == ConstantMaySetV.top)
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Set - SetFrom (Tuple)") {
//    val mod = Module("Test1", BaseIR.language + tupleIR + arithIR + setIR, Seq(
//      Relation("nums", Seq(
//        Param("x", TInt),
//        Param("y", TInt)
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), IntNum(1)),
//          Eq(Var("y"), IntNum(2))
//        )),
//        Body(Seq(
//          Eq(Var("x"), IntNum(3)),
//          Eq(Var("y"), IntNum(4))
//        ))
//      )),
//      Relation("main", Seq(
//        Param("x", TSet(TTuple(Seq(TInt, TInt))))
//      ), Seq(
//        Body(Seq(
//          Eq(Var("x"), SetFrom(RefByName("nums")))
//        ))
//      )).addHint(MainHint)
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("x"))
//    assert(mainRel.rows.head == ConstantMaySetV(ConstantTupleV(Value.Top, Value.Top)))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  /* Map */
//
//  test("Map - Empty") {
//    val mod = Module("Test1", BaseIR.language + mapIR, Seq(
//      Relation(
//        "main",
//        Seq(
//          Param("m", TMap(TNothing, TNothing))
//        ),
//        Seq(Body(Seq(
//          Eq(Var("m"), MapLit.empty)
//        )))
//      )
//    ))
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("m"))
//    assert(mainRel.rows == Seq(ConstantMayMapV.empty))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Map - Literal") {
//    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
//      Relation(
//        "main",
//        Seq(Param("m", TMap(TString, TInt))),
//        Seq(Body(Seq(
//          Eq(Var("m"), MapLit.from((StringLit("A"), IntNum(1)), (StringLit("B"), IntNum(2))))
//        )))
//      )
//    ))
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("m"))
//    assert(mainRel.rows == Seq(ConstantMayMapV(
//      ConstantStringV("A") -> Set(ConstantIntV(1)),
//      ConstantStringV("B") -> Set(ConstantIntV(2))
//    )))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Map - Union") {
//    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
//      Relation(
//        "main",
//        Seq(Param("m2", TMap(TString, TInt))),
//        Seq(Body(Seq(
//          Eq(Var("m1"), MapLit.from((StringLit("A"), IntNum(1)))),
//          Eq(Var("m2"), MapUnion(Var("m1"), MapLit.from((StringLit("B"), IntNum(2)))))
//        )))
//      )
//    ))
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("m2"))
//    assert(mainRel.rows == Seq(ConstantMayMapV(
//      ConstantStringV("A") -> Set(ConstantIntV(1)),
//      ConstantStringV("B") -> Set(ConstantIntV(2))
//    )))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Map - Union (Collision)") {
//    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
//      Relation(
//        "main",
//        Seq(Param("m2", TMap(TString, TInt))),
//        Seq(Body(Seq(
//          Eq(Var("m1"), MapLit.from((StringLit("A"), IntNum(1)))),
//          Eq(Var("m2"), MapUnion(Var("m1"), MapLit.from((StringLit("A"), IntNum(2)))))
//        )))
//      )
//    ))
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("m2"))
//    assert(mainRel.rows == Seq(ConstantMayMapV(
//      ConstantStringV("A") -> Set(ConstantIntV(1), ConstantIntV(2)),
//    )))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Map - From Relation") {
//    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
//      Relation(
//        "main",
//        Seq(Param("m", TMap(TString, TInt))),
//        Seq(Body(Seq(
//          Eq(Var("m"), MapFrom("someCall"))
//        )))
//      ),
//      Relation(
//        "someCall",
//        Seq(
//          Param("k", TDemand(TString)),
//          Param("v", TInt)
//        ),
//        Seq(
//          Body(Seq(
//            Eq(Var("k"), StringLit("A")),
//            Eq(Var("v"), IntNum(1))
//          )),
//          Body(Seq(
//            Eq(Var("k"), StringLit("B")),
//            Eq(Var("v"), IntNum(2))
//          ))
//        )
//      )
//    ))
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("m"))
//    // top, since we first evaluate the relation and then fill the map
//    assert(mainRel.rows == Seq(ConstantMayMapV.top))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Map - Comprehension") {
//    val mod = Module("Test1", BaseIR.language + arithIR + mapIR, Seq(
//      Relation(
//        "main",
//        Seq(Param("m2", TMap(TInt, TInt))),
//        Seq(Body(Seq(
//          Eq(Var("m1"), MapLit(Seq(
//            IntNum(1) -> IntNum(2),
//          ))),
//          Eq(Var("m2"), MapComprehension(Var("k"), Add(Var("v"), IntNum(1)), Seq(
//            MapContains(Var("m1"), Var("k")),
//            Eq(Var("v"), MapLookUp(Var("m1"), Var("k")))
//          )))
//        )))
//      )
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("m2"))
//    assert(mainRel.rows == Seq(ConstantMayMapV(
//      ConstantIntV(1) -> Set(ConstantIntV(3)),
//    )))
//    assertResult(Topped.Actual(false))(mainRel.empty)
//  }
//
//  test("Map - Comprehension (multiple key-value pairs)") {
//    val mod = Module("Test1", BaseIR.language + arithIR + mapIR, Seq(
//      Relation(
//        "main",
//        Seq(Param("m2", TMap(TInt, TInt))),
//        Seq(Body(Seq(
//          Eq(Var("m1"), MapLit(Seq(
//            IntNum(1) -> IntNum(2),
//            IntNum(2) -> IntNum(3)
//          ))),
//          Eq(Var("m2"), MapComprehension(Var("k"), Add(Var("v"), IntNum(1)), Seq(
//            MapContains(Var("m1"), Var("k")),
//            Eq(Var("v"), MapLookUp(Var("m1"), Var("k")))
//          )))
//        )))
//      )
//    ))
//
//    val constRes = interp(mod)
//    val mainRel = constRes("main")
//    assert(mainRel.cols == Seq("m2"))
//    assert(mainRel.rows == Seq(ConstantMayMapV.top))
//    assertResult(Topped.Top)(mainRel.empty)
//  }