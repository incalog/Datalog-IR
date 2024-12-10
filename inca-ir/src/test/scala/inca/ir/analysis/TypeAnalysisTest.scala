package inca.ir.analysis

import inca.ir.analysis.base.values.{TypeRelation, TypeValue}
import inca.ir.extension.arithmetic.{Add, IntNum, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Call, Eq, ExtensionalCall, ExtensionalRelation, Module, Param, Relation, Var, WildcardArg, string2name, termList2ArgList}
import org.scalatest.funsuite.AnyFunSuiteLike
import sturdy.values.Topped
import sturdy.values.Topped.Top


class TypeAnalysisTest extends AnyFunSuiteLike:

  def interp(mod: Module, edb: Map[String, TypeRelation] = Map()): Map[String, TypeRelation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val abstractInterp = IRTypeAbstractInterpreter()
    edb.foreach(abstractInterp.insertEDB)
    val res = abstractInterp.evalProgram(Seq(mod))
    abstractInterp.idb.getState.map(kv => kv._1.toString.drop(1) -> kv._2)

  test("Single relation") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("main", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Mul(Add(IntNum(3), IntNum(4)), IntNum(2)))
        ))
      )).addHint(MainHint)
    ))

    val relTypes = interp(mod)

    val mainRelType = relTypes("main")
    assert(mainRelType.cols == Seq("out"))
    assert(mainRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(mainRelType.empty)
  }

  test("Comparison") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("nums", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
        ))
      )),
      Relation("main", Seq(
        Param("x", TInt)
      ), Seq(
        Body(Seq(
          Call("nums", Seq(Var("x"))),
          Eq(Var("x"), Var("x"))
        ))
      )).addHint(MainHint)
    ))

    val relTypes = interp(mod)

    val numRelType = relTypes("nums")
    assert(numRelType.cols == Seq("x"))
    assert(numRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(numRelType.empty)

    val mainRelType = relTypes("main")
    assert(mainRelType.cols == Seq("x"))
    assert(mainRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainRelType.empty)
  }

  test("Comparison 2 ") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("xs", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(Eq(Var("x"), IntNum(1)))),
        Body(Seq(Eq(Var("x"), IntNum(2))))
      )),
      Relation("ys", Seq(
        Param("y", TInt),
      ), Seq(
        Body(Seq(Eq(Var("y"), IntNum(1)))),
        Body(Seq(Eq(Var("y"), IntNum(2)))),
        Body(Seq(Eq(Var("y"), IntNum(3))))
      )),
      Relation("main", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("xs", Seq(Var("x"))),
          Call("ys", Seq(Var("y"))),
          Eq(Var("x"), Sub(Var("y"), IntNum(1)))
        ))
      )).addHint(MainHint)
    ))

    val relTypes = interp(mod)

    val xsRelType = relTypes("xs")
    assert(xsRelType.cols == Seq("x"))
    assert(xsRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(xsRelType.empty)

    val ysRelType = relTypes("ys")
    assert(ysRelType.cols == Seq("y"))
    assert(ysRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(ysRelType.empty) // Top, since we are dataflow driven

    val mainRelType = relTypes("main")
    assert(mainRelType.cols == Seq("x", "y"))
    assert(mainRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainRelType.empty)
  }

  test("Comparison 3") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("xs", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("x"), IntNum(2)),
        ))
      )),
      Relation("ys", Seq(
        Param("y", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("y"), IntNum(1)),
        )),
        Body(Seq(
          Eq(Var("y"), IntNum(2)),
        )),
        Body(Seq(
          Eq(Var("y"), IntNum(3)),
        ))
      )),
      Relation("main", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("xs", Seq(Var("x"))),
          Call("ys", Seq(Var("y"))),
          Eq(Var("x"), Sub(Var("y"), IntNum(1)))
        ))
      )).addHint(MainHint)
    ))

    val relTypes = interp(mod)

    val xsRelType = relTypes("xs")
    assert(xsRelType.cols == Seq("x"))
    assert(xsRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(xsRelType.empty)

    val ysRelType = relTypes("ys")
    assert(ysRelType.cols == Seq("y"))
    assert(ysRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(ysRelType.empty)

    val mainRelType = relTypes("main")
    assert(mainRelType.cols == Seq("x", "y"))
    assert(mainRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainRelType.empty)
  }

  test("Two relations") {
    val mod = Module("Test2", BaseIR.language + arithIR, Seq(
      Relation("calc", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Mul(Add(IntNum(3), IntNum(4)), IntNum(2)))
        ))
      )),
      Relation("main", Seq(
        Param("res", TInt)
      ), Seq(
        Body(Seq(
          Call("calc", Seq(Var("res")))
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val calcRelType = relTypes("calc")
    assert(calcRelType.cols == Seq("out"))
    assert(calcRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(calcRelType.empty)

    val mainRelType = relTypes("main")
    assert(mainRelType.cols == Seq("res"))
    assert(mainRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainRelType.empty)
  }

  test("Right Recursion") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("path", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y")))
        )),
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("z"))),
          Call("path", Seq(Var("z"), Var("y"))),
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = relTypes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(pathRelType.empty)
  }

  test("Left Recursion") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("path", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y")))
        )),
        Body(Seq(
          Call("path", Seq(Var("x"), Var("z"))),
          Call("edge", Seq(Var("z"), Var("y"))),
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = relTypes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(pathRelType.empty)
  }

  test("Left and right Recursion") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("path", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y")))
        )),
        Body(Seq(
          Call("path", Seq(Var("x"), Var("z"))),
          Call("path", Seq(Var("z"), Var("y"))),
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = relTypes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(pathRelType.empty)
  }

  test("Left and right Recursion - Start query") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("path", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y")))
        )),
        Body(Seq(
          Call("path", Seq(Var("x"), Var("z"))),
          Call("path", Seq(Var("z"), Var("y"))),
        ))
      )),
      Relation("main", Seq(
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("path", Seq(IntNum(1), Var("y")))
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = relTypes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(pathRelType.empty)

    val mainRelType = relTypes("main")
    assert(mainRelType.cols == Seq("y"))
    assert(mainRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainRelType.empty)
  }

  test("Factorial") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("input", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("n$0"))),
          Eq(Var("n$0"), IntNum(1), true),
          Eq(Var("n"), Sub(Var("n$0"), IntNum(1)))
        )),
        Body(Seq(
          Eq(Var("n"), IntNum(3)),
        ))
      )),
      Relation("fac", Seq(
        Param("n", TInt),
        Param("r", TInt)
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(1)),
          Eq(Var("r"), IntNum(1))
        )),
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(1), true),
          Call("fac", Seq(Sub(Var("n"), IntNum(1)), Var("r$0"))),
          Eq(Var("r"), Mul(Var("n"), Var("r$0")))
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val inputRelType = relTypes("input")
    assert(inputRelType.cols == Seq("n"))
    assert(inputRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(inputRelType.empty)

    val facRelType = relTypes("fac")
    assert(facRelType.cols == Seq("n", "r"))
    assert(facRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(facRelType.empty)
  }

  test("Recursive prefix sum") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("input", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(Eq(Var("n"), IntNum(1)))),
        Body(Seq(Eq(Var("n"), IntNum(2)))),
        Body(Seq(Eq(Var("n"), IntNum(3))))
      )),
      Relation("prefixSum", Seq(
        Param("t", TInt),
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("t"))),
          Eq(Var("t"), IntNum(1)),
          Eq(Var("n"), IntNum(1)),
        )),
        Body(Seq(
          Call("input", Seq(Var("t"))),
          Eq(Var("t"), IntNum(1), true),
          Call("prefixSum", Seq(Sub(Var("t"), IntNum(1)), Var("s"))),
          Eq(Var("n"), Add(Var("s"), Var("t"))),
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val inputRelType = relTypes("input")
    assert(inputRelType.cols == Seq("n"))
    assert(inputRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(inputRelType.empty)

    val sumRelType = relTypes("prefixSum")
    assert(sumRelType.cols == Seq("t", "n"))
    assert(sumRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(sumRelType.empty)
  }

  test("Negative filter") {
    val mod = Module("Test", BaseIR.language + arithIR, Seq(
      Relation("input", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("n"), IntNum(1)),
        )),
        Body(Seq(
          Eq(Var("n"), IntNum(2)),
        )),
        Body(Seq(
          Eq(Var("n"), IntNum(3)),
        ))
      )),
      Relation("main", Seq(
        Param("t", TInt),
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("t"))),
          Eq(Var("t"), IntNum(1), true),
          Eq(Var("n"), Var("t")),
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val inputRelType = relTypes("input")
    assert(inputRelType.cols == Seq("n"))
    assert(inputRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(inputRelType.empty)

    val mainRelType = relTypes("main")
    assert(mainRelType.cols == Seq("t", "n"))
    assert(mainRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainRelType.empty)
  }

  test("Failing atom") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3)),
          Eq(Var("x"), Var("y")),
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(3)),
          Eq(Var("y"), IntNum(4)),
          Eq(Var("x"), Var("y"), true),
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)
  }

  test("Body Failing") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2)),
          Eq(Var("x"), Var("y"))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3)),
          Eq(Var("x"), Var("y"))
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)
  }

  test("EDB call") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("input_edge", Seq(
        Param("a", TInt),
        Param("b", TInt)
      )),
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          ExtensionalCall("input_edge", Seq(Var("x").arg, Var("y").arg)),
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod, Map(
      "input_edge" -> TypeRelation(Seq("a", "b"), Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)), Topped.Top)
    ))

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)
  }

  test("EDB call - Args bound") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("input_edge", Seq(
        Param("a", TInt),
        Param("b", TInt)
      )),
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          ExtensionalCall("input_edge", Seq(IntNum(1).arg, Var("y").arg)),
          Eq(Var("x"), IntNum(5))
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod, Map(
      "input_edge" -> TypeRelation(Seq("a", "b"), Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)), Topped.Top)
    ))

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)
  }

  test("EDB call - Negate") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("input_edge", Seq(
        Param("a", TInt),
        Param("b", TInt)
      )),
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          ExtensionalCall("input_edge", Seq(IntNum(1).arg, WildcardArg()), true),
          Eq(Var("x"), IntNum(5)),
          Eq(Var("y"), IntNum(6))
        )),
        Body(Seq(
          ExtensionalCall("input_edge", Seq(IntNum(1), IntNum(2)), true),
          Eq(Var("x"), IntNum(7)),
          Eq(Var("y"), IntNum(8))
        )),
        Body(Seq(
          ExtensionalCall("input_edge", Seq(IntNum(2), IntNum(2)), true),
          Eq(Var("x"), IntNum(9)),
          Eq(Var("y"), IntNum(10))
        )),
        Body(Seq(
          ExtensionalCall("input_edge", Seq(IntNum(4), IntNum(4)), true),
          Eq(Var("x"), IntNum(11)),
          Eq(Var("y"), IntNum(12))
        )),
        Body(Seq(
          ExtensionalCall("input_edge", Seq(WildcardArg(), IntNum(2).arg), true),
          Eq(Var("x"), IntNum(13)),
          Eq(Var("y"), IntNum(14))
        ))
      )).addHint(MainHint),
    ))

    // 9	10
    // 11	12

    val relTypes = interp(mod, Map(
      "input_edge" -> TypeRelation(Seq("a", "b"), Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)), Topped.Top)
    ))

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)
  }

  test("Call - negative") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2)),
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3)),
        ))
      )),
      Relation("filterEdge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(IntNum(3).arg, WildcardArg()), true),
          Eq(Var("x"), IntNum(4)),
          Eq(Var("y"), IntNum(5)),
        ))
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val edgeRelType = relTypes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(edgeRelType.empty)

    val filterEdgeRelType = relTypes("filterEdge")
    assert(filterEdgeRelType.cols == Seq("x", "y"))
    assert(filterEdgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(filterEdgeRelType.empty)
  }

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

    val relTypes = interp(mod)

    val helperRelType = relTypes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = relTypes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows == Seq(TypeValue.AType(TData("TList"))))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }

  test("ADT - Deconstruct") {
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
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Call("helper", Seq(Var("y"))),
          Eq(Var("z"), Construct("TCons", Seq(Var("y"), Construct("TNil", Seq())))),
          Deconstruct(Var("z"), "TCons", Seq(Var("x").arg, WildcardArg()), false)
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val helperRelType = relTypes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = relTypes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }

  test("ADT - Deconstruct as filter") {
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
          Deconstruct(Var("x"), "TCons", Seq(IntNum(1).arg, WildcardArg()), false)
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val helperRelType = relTypes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = relTypes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows == Seq(TypeValue.AType(TData("TList"))))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
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

    val relTypes = interp(mod)

    val helperRelType = relTypes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(TypeValue.AType(TData("TList"))))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = relTypes("main")
    assert(mainEdgeRelType.cols == Seq("x", "y"))
    assert(mainEdgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TData("TList"))))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }

  test("ADT - Deconstruct dispatch filter") {
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
        Param("x", TInt), Param("y", TData("TList")),
      ), Seq(
        Body(Seq(
          Call("helper", Seq(Var("y"))),
          Deconstruct(Var("y"), "TCons", Seq(IntNum(8).arg, WildcardArg()), false),
          Eq(Var("x"), IntNum(10))
        )),
        Body(Seq(
          Call("helper", Seq(Var("y"))),
          Deconstruct(Var("y"), "TNil", Seq(), false),
          Eq(Var("x"), IntNum(-1))
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val helperRelType = relTypes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(TypeValue.AType(TData("TList"))))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = relTypes("main")
    assert(mainEdgeRelType.cols == Seq("x", "y"))
    assert(mainEdgeRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TData("TList"))))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }

  test("ADT - Deconstruct negative as filter") {
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
          Deconstruct(Var("x"), "TCons", Seq(IntNum(1).arg, WildcardArg()), true)
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val helperRelType = relTypes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = relTypes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows == Seq(TypeValue.AType(TData("TList"))))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }

  test("Mutual Recursion, multiple call sites") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("input_calc", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Call("input_calc", Seq(IntNum(1))),
          Call("calc", Seq(IntNum(1), Var("elem"))),
          Eq(Var("x"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(1))
        ))
      )),
      Relation("calc", Seq(
        Param("x", TInt),
        Param("elem", TInt)
      ), Seq(
        Body(Seq(
          Call("input_calc", Seq(Var("x"))),
          Eq(Var("x"), IntNum(1)),
          Eq(Var("elem"), IntNum(3))
        )),
        Body(Seq(
          Call("input_calc", Seq(Var("x"))),
          Eq(Var("x"), IntNum(2)),
          Call("calc", Seq(IntNum(1), Var("elem")))
        )),
      )),
      Relation("main", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Call("calc", Seq(IntNum(2), Var("x")))
        )),
      )).addHint(MainHint),
    ))

    val relTypes = interp(mod)

    val inputCalcEdgeRelType = relTypes("input_calc")
    assert(inputCalcEdgeRelType.cols == Seq("x"))
    assert(inputCalcEdgeRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(inputCalcEdgeRelType.empty)

    val calcRelType = relTypes("calc")
    assert(calcRelType.cols == Seq("x", "elem"))
    assert(calcRelType.rows == Seq(TypeValue.AType(TInt), TypeValue.AType(TInt)))
    assertResult(Topped.Top)(calcRelType.empty)

    val mainEdgeRelType = relTypes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows == Seq(TypeValue.AType(TInt)))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }

