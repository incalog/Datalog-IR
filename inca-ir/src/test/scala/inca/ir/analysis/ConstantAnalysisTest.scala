package inca.ir.analysis

import inca.ir.analysis.base.values.{ConstantRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
import inca.ir.extension.arithmetic.{Add, IntNum, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.bool.analysis.interpreter.ConstantBoolV
import inca.ir.extension.bool.{AtomAsBool, BoolFalse, BoolTrue, TBoolean}
import inca.ir.extension.data.analysis.interpreter.ConstantDataV
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
import inca.ir.extension.tuple.analysis.interpreter.ConstantTupleV
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.printer.IRDebugPrinter
import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Call, Eq, ExtensionalCall, ExtensionalRelation, MainHint, Module, Param, Relation, Var, WildcardArg, string2name, termList2ArgList}
import org.scalatest.funsuite.AnyFunSuiteLike
import sturdy.values.Topped

class ConstantAnalysisTest extends AnyFunSuiteLike:

  def interp(mod: Module, edb: Map[String, ConstantRelation] = Map()): Map[String, ConstantRelation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val abstractInterp = IRConstantAbstractInterpreter(interRelational = true)
    edb.foreach(abstractInterp.insertEDB)
    abstractInterp.evalProgram(Seq(mod))
    val res = abstractInterp.getIDB

    //val printer = new IRDebugPrinter {}
    //println(printer.prettyPrint(mod))

    res

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

    val constRes = interp(mod)

    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("out"))
    assert(mainRelType.rows == Seq(ConstantIntV(14)))
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

    val constRes = interp(mod)

    val numRelType = constRes("nums")
    assert(numRelType.cols == Seq("x"))
    assert(numRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(numRelType.empty)

    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("x"))
    assert(mainRelType.rows == Seq(Value.Top))
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

    val constRes = interp(mod)

    val xsRelType = constRes("xs")
    assert(xsRelType.cols == Seq("x"))
    assert(xsRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(xsRelType.empty)

    val ysRelType = constRes("ys")
    assert(ysRelType.cols == Seq("y"))
    assert(ysRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(ysRelType.empty)

    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("x", "y"))
    assert(mainRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    // This already fails
    val xsRelType = constRes("xs")
    assert(xsRelType.cols == Seq("x"))
    assert(xsRelType.empty == Topped.Actual(true))
    assertResult(Topped.Actual(true))(xsRelType.empty)

    // Therefore this fails as well and ys is never evaluated
    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("x", "y"))
    assert(mainRelType.empty == Topped.Actual(true))
    assertResult(Topped.Actual(true))(mainRelType.empty)
  }

  test("Comparison 4 ") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("xs", Seq(
        Param("x", TInt),
        Param("y", TInt),
      ), Seq(
        Body(Seq(Eq(Var("x"), IntNum(1)), Eq(Var("y"), IntNum(2)))),
        Body(Seq(Eq(Var("x"), IntNum(2)), Eq(Var("y"), IntNum(3))))
      )),
      Relation("main", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("xs", Seq(Var("x"), IntNum(2))),
          Call("xs", Seq(Var("y"), IntNum(3))),
          Eq(Var("x"), Var("y"))
        ))
      )).addHint(MainHint)
    ))

    val constRes = interp(mod)

    val xsRelType = constRes("xs")
    assert(xsRelType.cols == Seq("x", "y"))
    assert(xsRelType.rows == Seq(Value.Top, Value.Top))
    assertResult(Topped.Actual(false))(xsRelType.empty)

    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("x", "y"))
    assertResult(Topped.Actual(true))(mainRelType.empty)
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

    val constRes = interp(mod)

    val calcRelType = constRes("calc")
    assert(calcRelType.cols == Seq("out"))
    assert(calcRelType.rows == Seq(ConstantIntV(14)))
    assertResult(Topped.Actual(false))(calcRelType.empty)

    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("res"))
    assert(mainRelType.rows == Seq(ConstantIntV(14)))
    assertResult(Topped.Actual(false))(mainRelType.empty)
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

    val constRes = interp(mod)

    val edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, Value.Top))
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = constRes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, Value.Top))
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = constRes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, Value.Top))
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = constRes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, Value.Top)) // we are demand driven
    assertResult(Topped.Top)(edgeRelType.empty)

    val pathRelType = constRes("path")
    assert(pathRelType.cols == Seq("x", "y"))
    assert(pathRelType.rows == Seq(Value.Top, Value.Top))
    assertResult(Topped.Top)(pathRelType.empty)

    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("y"))
    assert(mainRelType.rows == Seq(Value.Top))
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

    val constRes = interp(mod)

    val inputRelType = constRes("input")
    assert(inputRelType.cols == Seq("n"))
    assert(inputRelType.rows == Seq(Value.Top))
    assertResult(Topped.Top)(inputRelType.empty)

    val facRelType = constRes("fac")
    assert(facRelType.cols == Seq("n", "r"))
    assert(facRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val inputRelType = constRes("input")
    assert(inputRelType.cols == Seq("n"))
    assert(inputRelType.rows == Seq(Value.Top))
    assertResult(Topped.Top)(inputRelType.empty)

    val sumRelType = constRes("prefixSum")
    assert(sumRelType.cols == Seq("t", "n"))
    assert(sumRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val inputRelType = constRes("input")
    assert(inputRelType.cols == Seq("n"))
    assert(inputRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(inputRelType.empty)

    val mainRelType = constRes("main")
    assert(mainRelType.cols == Seq("t", "n"))
    assert(mainRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, Value.Top))
    assertResult(Topped.Actual(false))(edgeRelType.empty)
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

    val constRes = interp(mod)

    val edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.empty == Topped.Actual(true))
    assertResult(Topped.Actual(true))(edgeRelType.empty)
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

    var constRes = interp(mod, Map(
      "input_edge" -> ConstantRelation(Seq("a", "b"), Seq(ConstantIntV(1), ConstantIntV(2)), Topped.Actual(false))
    ))

    var edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(ConstantIntV(1), ConstantIntV(2)))
    assertResult(Topped.Actual(false))(edgeRelType.empty)

    constRes = interp(mod, Map(
      "input_edge" -> ConstantRelation(Seq("a", "b"), Seq(Value.Top, ConstantIntV(2)), Topped.Actual(false))
    ))

    edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, ConstantIntV(2)))
    assertResult(Topped.Actual(false))(edgeRelType.empty)
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

    var constRes = interp(mod, Map(
      "input_edge" -> ConstantRelation(Seq("a", "b"), Seq(ConstantIntV(1), ConstantIntV(2)), Topped.Actual(false))
    ))

    var edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(ConstantIntV(5), ConstantIntV(2)))
    assertResult(Topped.Actual(false))(edgeRelType.empty)

    constRes = interp(mod, Map(
      "input_edge" -> ConstantRelation(Seq("a", "b"), Seq(Value.Top, ConstantIntV(2)), Topped.Actual(false))
    ))

    edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(ConstantIntV(5), ConstantIntV(2)))
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

    var constRes = interp(mod, Map(
      "input_edge" -> ConstantRelation(Seq("a", "b"), Seq(ConstantIntV(1), ConstantIntV(2)), Topped.Actual(false))
    ))

    var edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, Value.Top))
    assertResult(Topped.Actual(false))(edgeRelType.empty)

    constRes = interp(mod, Map(
      "input_edge" -> ConstantRelation(Seq("a", "b"), Seq(Value.Top, ConstantIntV(2)), Topped.Actual(false))
    ))

    edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.rows == Seq(Value.Top, Value.Top))
    //println(edgeRelType.empty)
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
    
    val constRes = interp(mod)

    val edgeRelType = constRes("edge")
    assert(edgeRelType.cols == Seq("x", "y"))
    assert(edgeRelType.empty == Topped.Actual(true))
    assertResult(Topped.Actual(true))(edgeRelType.empty)

    val filterEdgeRelType = constRes("filterEdge")
    assert(filterEdgeRelType.cols == Seq("x", "y"))
    assert(filterEdgeRelType.rows == Seq(ConstantIntV(4), ConstantIntV(5)))
    assertResult(Topped.Actual(false))(filterEdgeRelType.empty)
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

    val constRes = interp(mod)

    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows.head.toString == "TCons(Top,TNil())")
    assertResult(Topped.Actual(false))(mainEdgeRelType.empty)
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

    val constRes = interp(mod)

    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows ==  Seq(Value.Top))
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

    val constRes = interp(mod)

    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows.head.toString == "TCons(Top,TNil())")
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

    val constRes = interp(mod)

    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x", "y"))
    assert(mainEdgeRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x", "y"))
    assert(mainEdgeRelType.rows == Seq(Value.Top, Value.Top))
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

    val constRes = interp(mod)

    val helperRelType = constRes("helper")
    assert(helperRelType.cols == Seq("x"))
    assert(helperRelType.rows == Seq(Value.Top))
    assertResult(Topped.Actual(false))(helperRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows.head.toString == "TCons(Top,TNil())")
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

    val constRes = interp(mod)
    
    val inputCalcEdgeRelType = constRes("input_calc")
    assert(inputCalcEdgeRelType.cols == Seq("x"))
    assert(inputCalcEdgeRelType.rows == Seq(Value.Top))
    assertResult(Topped.Top)(inputCalcEdgeRelType.empty)

    val calcRelType = constRes("calc")
    assert(calcRelType.cols == Seq("x", "elem"))
    assert(calcRelType.rows == Seq(Value.Top, ConstantIntV(3)))
    assertResult(Topped.Top)(calcRelType.empty)

    val mainEdgeRelType = constRes("main")
    assert(mainEdgeRelType.cols == Seq("x"))
    assert(mainEdgeRelType.rows == Seq(ConstantIntV(3)))
    assertResult(Topped.Top)(mainEdgeRelType.empty)
  }

  /* Tuple */

  test("Tuple - Single relation") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("main", Seq(
        Param("out1", TTuple(Seq(TInt, TInt))),
        Param("out2", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("out1"), TupleLit(Seq(IntNum(1), IntNum(2)))),
          Eq(Var("out2"), Project(Var("out1"), 1))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRel = res("main")
    assert(mainRel.cols == Seq("out1", "out2"))
    assert(mainRel.rows == Seq(ConstantTupleV(Seq(ConstantIntV(1), ConstantIntV(2))), ConstantIntV(2)))
    assertResult(Topped.Actual(false))(mainRel.empty)
  }

  /* Boolean */

  test("Boolean - AtomAsBool failing") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("main", Seq(
        Param("out", TBoolean),
      ), Seq(
        Body(Seq(
          Eq(Var("out"), AtomAsBool(
            Call("fail", Seq(WildcardArg())))
          )
        ))
      )).addHint(MainHint),
      Relation("fail", Seq(
        Param("x", TBoolean),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), BoolTrue),
          Eq(Var("x"), BoolFalse),
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRel = res("main")
    assert(mainRel.cols == Seq("out"))
    assert(mainRel.rows == Seq(ConstantBoolV(false)))
    assertResult(Topped.Actual(false))(mainRel.empty)
  }

