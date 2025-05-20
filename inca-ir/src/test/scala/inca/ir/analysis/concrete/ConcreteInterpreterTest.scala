package inca.ir.analysis.concrete

import inca.ir.execution.interpreter.{Executor, InterpreterRelation}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, map, not, set, tuple, typeparam}
import inca.ir.extension.arithmetic.analysis.interpreter.CIntV
import inca.ir.extension.arithmetic.{Add, ArithmeticAggregationOperator, IntNum, LT, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.string.{StringConcat, StringLit, TString, IR as stringIR}
import inca.ir.extension.block.{Block, IR as blockIR}
import inca.ir.extension.bool.{AtomAsBool, BoolFalse, BoolTrue, TBoolean, IR as boolIR}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
import inca.ir.extension.datamatch.{Case, Match, IR as datamatchIR}
import inca.ir.extension.demand.{TDemand, IR as demandIR}
import inca.ir.extension.disjunction.{Disjunction, IR as disjunctionIR}
import inca.ir.extension.map.{MapComprehension, MapContains, MapFrom, MapFun, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.not.{Not, IR as notIR}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, SyntacticOptimizer, TSet, IR as setIR}
import inca.ir.extension.tuple.{Project, TTuple, TupleLit, IR as tupleIR}
import inca.ir.extension.impure.{Impure, ImpurityKind, IR as impureIR}
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.hints.MainHint
import inca.ir.typing.IRTypechecker
import inca.ir.util.SourceLocation
import inca.ir.{Arg, BaseIR, Body, Call, CompiledUnit, Eq, ExtensionalCall, ExtensionalRelation, Module, Name, Param, RefByName, Relation, TNothing, Type, Var, WildcardArg, execution, string2name, term2Arg, termList2ArgList}
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuiteLike

private case class CompiledInterpreterTestUnit(mod: Module) extends CompiledUnit:
  setPipeline(Nil)
  /*setPipeline(
    List(
      () => new set.Lowering {},
      () => new map.Lowering {},
      () => new bool.Lowering {},
      () => new datamatch.Lowering {},
      () => new block.Lowering {},
      () => new impure.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {},
    ) // arith + string + data
  )*/

  override def compilerOptions: CompilerOptions = CompilerOptions.default
  override def name: Name = mod.name
  override def sourceLocation: SourceLocation = mod.name
  override def isClosedWorld: Boolean = true
  override val irModules: Seq[Module] = Seq(mod)
  override val otherUnits: Seq[CompiledUnit] = Seq()


class ConcreteInterpreterTest extends AnyFunSuiteLike:

  def interp(mod: Module, edb: Seq[execution.Relation] = Seq()): Map[String, execution.Relation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val interp = new Executor
    val compiled = CompiledInterpreterTestUnit(mod)
    val engine = interp.instantiate(compiled)
    edb.foreach(engine.insert)
    engine.readAll().map(r => r.name -> r).toMap

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

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == 14)
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

    val res = interp(mod)
    assert(res("main").size == 2)
  }

  test("Comparison 2 ") {
    val mod = Module("Test1", BaseIR.language + arithIR + dataIR, Seq(
      Relation("xs", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
        )),
        Body(Seq(
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

    val res = interp(mod)
    assert(res("main").size == 2)
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

    val rels = interp(mod)
    rels.foreach(r => assert(r._2.isEmpty))
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

    val res = interp(mod)
    assert(res("calc").size == 1)
    assert(res("main").size == 1)
    assert(res("calc").entries.head == 14)
    assert(res("main").entries.head == 14)
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

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(2, 3)))

    val pathRel = res("path")
    assert(pathRel.size == 3)
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(2, 3)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 3)))
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

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(2, 3)))

    val pathRel = res("path")
    assert(pathRel.size == 3)
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(2, 3)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 3)))
  }

  test("Left Recursion - Subquery") {
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
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(3)),
          Eq(Var("y"), IntNum(1))
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
      )),
      Relation("main", Seq(
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("path", Seq(IntNum(1), Var("y")))
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 3)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(2, 3)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(3, 1)))

    val pathRel = res("main")
    assert(pathRel.size == 3)
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(2)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(3)))
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

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(2, 3)))

    val pathRel = res("path")
    assert(pathRel.size == 3)
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(2, 3)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 3)))
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

    val res = interp(mod)

    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(2, 3)))

    val pathRel = res("path")
    assert(pathRel.size == 3)
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(2, 3)))
    assert(pathRel.entries.map(pathRel.flattenEntry).toSet.contains(Seq(1, 3)))

    val mainRel = res("main")
    assert(mainRel.size == 2)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(2)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(3)))
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
      /*Relation("main", Seq(
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("fac", Seq(IntNum(3), Var("y")))
        )),
      )).addHint(MainHint)*/
    ))
    
    val res = interp(mod)
    val mainRel = res("fac")
    assert(mainRel.size == 3)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(1, 1)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(2, 2)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(3, 6)))
  }

  test("Factorial - Main method") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("ext_main$input", Seq(Param("n", TInt))),
      Relation("fact", Seq(
        Param("n", TInt),
        Param("fact_result$0", TInt)
      ), Seq(
        Body(Seq(
          Call("fact$input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(1)),
          Eq(Var("fact_result$0"), IntNum(1))
        )),
        Body(Seq(
          Call("fact$input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(1), true),
          Call("fact", Seq(Sub(Var("n"), IntNum(1)), Var("fact_call$0"))),
          Eq(Var("fact_result$0"), Mul(Var("n"), Var("fact_call$0")))
        )),
      )),
      Relation("main", Seq(
        Param("n", TInt),
        Param("main_result$0", TInt)
      ), Seq(
        Body(Seq(
          ExtensionalCall("ext_main$input", Seq(Var("n"))),
          Call("fact", Seq(Var("n"), Var("main_result$0")))
        )),
      )).addHint(MainHint),
      Relation("fact$input", Seq(
        Param("n$0", TInt),
      ), Seq(
        Body(Seq(
          Call("fact$input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(1), true),
          Eq(Var("n$0"), Sub(Var("n"), IntNum(1)))
        )),
        Body(Seq(
          //Eq(Var("n"), IntNum(5)),
          ExtensionalCall("ext_main$input", Seq(Var("n$0"))),
        ))
      ))
    ))

    val res = interp(mod, Seq(execution.Relation1("ext_main$input", Seq("param_0"), Seq(Seq(5)))))
    val mainRel = res("main")
    assert(mainRel.size == 1)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(5, 120)))
  }

  test("Fibonacci") {
    val input_n = 7

    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("input", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("n$0"))),
          Eq(Var("n$0"), IntNum(0), true),
          Eq(Var("n"), Sub(Var("n$0"), IntNum(1)))
        )),
        Body(Seq(
          Eq(Var("n"), IntNum(input_n)),
        ))
      )),
      Relation("fib", Seq(
        Param("n", TInt),
        Param("r", TInt)
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(0)),
          Eq(Var("r"), IntNum(0))
        )),
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(1)),
          Eq(Var("r"), IntNum(1))
        )),
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(0), true),
          Eq(Var("n"), IntNum(1), true),
          Call("fib", Seq(Sub(Var("n"), IntNum(1)), Var("r$0"))),
          Call("fib", Seq(Sub(Var("n"), IntNum(2)), Var("r$1"))),
          Eq(Var("r"), Add(Var("r$0"), Var("r$1")))
        )),
      )),
      Relation("main", Seq(
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("fib", Seq(IntNum(input_n), Var("y")))
        )),
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRel = res("main")
    assert(mainRel.size == 1)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(13)))
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

    val res = interp(mod)
    val mainRel = res("prefixSum")
    assert(mainRel.size == 3)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(1, 1)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(2, 3)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(3, 6)))
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

    val res = interp(mod)
    val mainRel = res("main")
    assert(mainRel.size == 2)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(2, 2)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(3, 3)))
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

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(3, 4)))
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

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 0)
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
          ExtensionalCall("input_edge", Seq(Var("x"), Var("y"))),
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod, Seq(
      execution.Relation.from("input_edge", Seq("a", "b"), Seq(Seq(1, 2), Seq(3, 4)))
    ))

    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(3, 4)))
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
          ExtensionalCall("input_edge", Seq(IntNum(1), Var("y"))),
          Eq(Var("x"), IntNum(5))
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod, Seq(
      execution.Relation.from("input_edge", Seq("a", "b"), Seq(Seq(1, 2), Seq(3, 4)))
    ))

    val edgeRel = res("edge")
    assert(edgeRel.size == 1)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(5, 2)))
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

    val res = interp(mod, Seq(
      execution.Relation.from("input_edge", Seq("a", "b"), Seq(Seq(1, 2), Seq(3, 2)))
    ))

    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(9, 10)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(11, 12)))
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

    val res = interp(mod)

    val edgeRel = res("filterEdge")
    assert(edgeRel.size == 1)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(4, 5)))
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

    val res = interp(mod)
    assert(res("main").size == 2)
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

    val res = interp(mod)
    assert(res("main").size == 2)
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

    val res = interp(mod)
    assert(res("main").size == 1)
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
        Param("x", TInt),Param("y", TData("TList")),
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

    val res = interp(mod)
    val mainRes = res("main").asInstanceOf[InterpreterRelation].table
    assert(mainRes.size == 3)
    assert(mainRes.rows.map(t => t.head) == Set(CIntV(-1), CIntV(5), CIntV(8)))
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

    val res = interp(mod)
    val mainRes = res("main").asInstanceOf[InterpreterRelation].table
    assert(mainRes.size == 2)
    assert(mainRes.rows.map(t => t.head) == Set(CIntV(-1), CIntV(10)))
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

    val res = interp(mod)
    assert(res("main").size == 1)
  }

  // The evaluation context of the recursive input_calc function is wrong after joining.
  // Making the fixpoint parameter-sensitive could solve this problem.
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

    val res = interp(mod)
    assert(res("main").size == 1)
  }

  test("Mutual Recursion, multiple call sites 2") {
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
        )),
        // This body, which does not contribute anything breaks the fixpoint algorithm
        Body(Seq(
          Call("calc", Seq(IntNum(2), Var("elem"))),
          Eq(Var("x"), IntNum(3))
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
        ))
      )),
      Relation("main", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Call("calc", Seq(IntNum(2), Var("x")))
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
  }

  test("Aggregate - non recursive") {
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
      Relation("main", Seq(
        Param("x", TInt),
        Param("y", TInt),
      ), Seq(
        Body(Seq(
          Aggregate(Name("edge"), Seq(WildcardArg(), AggregateColumnArg(Var("y"))), ArithmeticAggregationOperator.MinInt),
          Call("edge", Seq(Var("x"), Var("y")))
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(2, 3)))

    val mainRel = res("main")
    assert(mainRel.size == 1)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(1, 2)))
  }

  test("Aggregate - non recursive (filtered)") {
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
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(3))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(4))
        ))
      )),
      Relation("main", Seq(
        Param("x", TInt),
        Param("y", TInt),
      ), Seq(
        Body(Seq(
          Aggregate(Name("edge"), Seq(IntNum(1).arg, AggregateColumnArg(Var("y"))), ArithmeticAggregationOperator.MinInt),
          Call("edge", Seq(Var("x"), Var("y")))
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 3)))

    val mainRel = res("main")
    assert(mainRel.size == 1)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(1, 2)))
  }

  test("Aggregate - count non recursive") {
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
      Relation("main", Seq(
        Param("x", TInt),
      ), Seq(
        Body(Seq(
          Aggregate(Name("edge"), Seq(WildcardArg(), AggregateColumnArg(Var("x"))), ArithmeticAggregationOperator.Count),
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    res.foreach(r => println(r._2.asTable))
    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(1, 2)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(2, 3)))

    val mainRel = res("main")
    assert(mainRel.size == 1)
    assert(mainRel.entries.head == 2)
  }

  /* demand */

  test("Fibonacci - demand input") {
    val input_n = 3

    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("input", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("n$0"))),
          Eq(Var("n$0"), IntNum(0), true),
          Eq(Var("n$0"), IntNum(1), true),
          Call("fib", Seq(Sub(Var("n$0"), IntNum(1)), Var("r"))),
          Eq(Var("n"), Sub(Var("n$0"), IntNum(2)))
        )),
        Body(Seq(
          Eq(Var("n"), IntNum(input_n)),
        )),
        Body(Seq(
          Call("input", Seq(Var("n$0"))),
          Eq(Var("n$0"), IntNum(0), true),
          Eq(Var("n$0"), IntNum(1), true),
          Eq(Var("n"), Sub(Var("n$0"), IntNum(1)))
        ))
      )),
      Relation("fib", Seq(
        Param("n", TInt),
        Param("r", TInt)
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(0)),
          Eq(Var("r"), IntNum(0))
        )),
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(1)),
          Eq(Var("r"), IntNum(1))
        )),
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Eq(Var("n"), IntNum(0), true),
          Eq(Var("n"), IntNum(1), true),
          Call("fib", Seq(Sub(Var("n"), IntNum(1)), Var("r$0"))),
          Call("fib", Seq(Sub(Var("n"), IntNum(2)), Var("r$1"))),
          Eq(Var("r"), Add(Var("r$0"), Var("r$1")))
        )),
      )),
      Relation("main", Seq(
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("fib", Seq(IntNum(input_n), Var("y")))
        )),
      )).addHint(MainHint)
    ))
    val res = interp(mod)
    val mainRel = res("main")
    assert(mainRel.size == 1)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(2)))
  }


  /* Tuple */

  test("Tuple - Single relation") {
    val mod = Module("Test1", BaseIR.language + arithIR + tupleIR, Seq(
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
    val firstEntry = mainRel.flattenEntry(mainRel.entries.head)
    assert(firstEntry.head == Seq(1, 2))
    assert(firstEntry.last.asInstanceOf[Int] == 2)
  }

  test("Tuple - Call argument") {
    val mod = Module("Test1", BaseIR.language + arithIR + tupleIR, Seq(
      Relation("helper", Seq(
        Param("out1", TTuple(Seq(TInt, TInt))),
      ), Seq(
        Body(Seq(
          Eq(Var("out1"), TupleLit(Seq(IntNum(1), IntNum(2)))),
        ))
      )),
      Relation("main", Seq(
        Param("success", TInt),
      ), Seq(
        Body(Seq(
          Call("helper", Seq(TupleLit(Seq(IntNum(1), IntNum(2))))),
          Eq(Var("success"), IntNum(1))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRel = res("main")
    val entry = mainRel.flattenEntry(mainRel.entries.head)
    assert(entry.head.asInstanceOf[Int] == 1)
  }

  test("Tuple - Unpacking in call argument") {
    val mod = Module("Test1", BaseIR.language + arithIR + tupleIR, Seq(
      Relation("helper", Seq(
        Param("out1", TTuple(Seq(TInt, TInt))),
      ), Seq(
        Body(Seq(
          Eq(Var("out1"), TupleLit(Seq(IntNum(1), IntNum(2)))),
        )),
        Body(Seq(
          Eq(Var("out1"), TupleLit(Seq(IntNum(2), IntNum(3)))),
        ))
      )),
      Relation("main", Seq(
        Param("x", TInt)
      ), Seq(
        Body(Seq(
          Call("helper", Seq(TupleLit(Seq(IntNum(1), Var("x"))))),
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRel = res("main")
    assert(mainRel.entries.size == 1)
    assert(mainRel.entries.head.asInstanceOf[Int] == 2)
  }

  /* Boolean */

  test("Boolean - AtomAsBool failing") {
    val mod = Module("Test1", BaseIR.language + arithIR + boolIR, Seq(
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
    assert(mainRel.entries.head == false)
  }

  /* Demand */

  test("Demand - Double Num") {
    val mod = Module("Test2", BaseIR.language + arithIR + demandIR, Seq(
      Relation("double", Seq(
        Param("in", TDemand(TInt)),
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Mul(Var("in"), IntNum(2)))
        ))
      )),
      Relation("main", Seq(
        Param("res", TInt)
      ), Seq(
        Body(Seq(
          Call("double", Seq(IntNum(5), Var("res")))
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    assert(res("main").entries.head == 10)
    val doubleRel = res("double")
    val firstEntry = doubleRel.flattenEntry(doubleRel.entries.head)
    assert(firstEntry.head.asInstanceOf[Int] == 5)
    assert(firstEntry.last.asInstanceOf[Int] == 10)
  }

  test("Demand - Recursive") {
    val mod = Module("Test3", BaseIR.language + stringIR + demandIR + arithIR + dataIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("count", Seq(
        Param("x", TDemand(TData("TList"))),
        Param("prevC", TDemand(TString)),
        Param("c", TString)
      ), Seq(
        Body(Seq(
          Deconstruct(Var("x"), "TNil", Seq()),
          Eq(Var("c"), StringConcat(Var("prevC"), StringLit("N")))
        )),
        Body(Seq(
          Deconstruct(Var("x"), "TCons", Seq(Var("hd"), Var("tail"))),
          Eq(Var("tmp"), StringConcat(Var("prevC"), StringLit("C"))),
          Call("count", Seq(Var("tail"), Var("tmp"), Var("c")))
        ))
      )),

      Relation("main", Seq(
        Param("c", TString),
      ), Seq(
        Body(Seq(
          Eq(Var("z"), Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))),
          Call("count", Seq(Var("z"), StringLit(""), Var("c")))
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == "CN")
  }

  /* Negate */

  test("Not - Filter") {
    val mod = Module("Test", BaseIR.language + arithIR + notIR, Seq(
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
          Not(Eq(Var("t"), IntNum(1))),
          Eq(Var("n"), Var("t")),
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val mainRel = res("main")
    assert(mainRel.size == 2)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(2, 2)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(3, 3)))
  }

  test("Not - EDB call") {
    val mod = Module("Test3", BaseIR.language + arithIR + notIR, Seq(
      ExtensionalRelation("input_edge", Seq(
        Param("a", TInt),
        Param("b", TInt)
      )),
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Not(ExtensionalCall("input_edge", Seq(IntNum(1).arg, WildcardArg()))),
          Eq(Var("x"), IntNum(5)),
          Eq(Var("y"), IntNum(6))
        )),
        Body(Seq(
          Not(ExtensionalCall("input_edge", Seq(IntNum(1), IntNum(2)))),
          Eq(Var("x"), IntNum(7)),
          Eq(Var("y"), IntNum(8))
        )),
        Body(Seq(
          Not(ExtensionalCall("input_edge", Seq(IntNum(2), IntNum(2)))),
          Eq(Var("x"), IntNum(9)),
          Eq(Var("y"), IntNum(10))
        )),
        Body(Seq(
          Not(ExtensionalCall("input_edge", Seq(IntNum(4), IntNum(4)))),
          Eq(Var("x"), IntNum(11)),
          Eq(Var("y"), IntNum(12))
        )),
        Body(Seq(
          Not(ExtensionalCall("input_edge", Seq(WildcardArg(), IntNum(2).arg))),
          Eq(Var("x"), IntNum(13)),
          Eq(Var("y"), IntNum(14))
        ))
      )).addHint(MainHint),
    ))

    // 9	10
    // 11	12

    val res = interp(mod, Seq(
      execution.Relation.from("input_edge", Seq("a", "b"), Seq(Seq(1, 2), Seq(3, 2)))
    ))

    val edgeRel = res("edge")
    assert(edgeRel.size == 2)
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(9, 10)))
    assert(edgeRel.entries.map(edgeRel.flattenEntry).toSet.contains(Seq(11, 12)))
  }

  /* Disjunction */

  test("Disjunction - Comparison") {
    val mod = Module("Test1", BaseIR.language + arithIR + disjunctionIR, Seq(
      Relation("main", Seq(
        Param("x", TInt)
      ), Seq(
        Body(Seq(
          Disjunction(Seq(
            Eq(Var("x"), IntNum(1))
          ), Seq(
            Eq(Var("x"), IntNum(2))
          ))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 2)
  }

  test("Disjunction - Not") {
    val mod = Module("Test1", BaseIR.language + disjunctionIR + notIR + arithIR, Seq(
      Relation("main", Seq(
        Param("x", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Disjunction(Seq(
            Eq(Var("y"), IntNum(0)),
            Eq(Var("y"), IntNum(0))
          ), Seq(
            Eq(Var("y"), IntNum(0)),
            Not(Eq(Var("y"), IntNum(0)))
          ))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head.asInstanceOf[Int] == 1)
  }

  test("Disjunction - Factorial") {
    val mod = Module("Test3", BaseIR.language + disjunctionIR + arithIR, Seq(
      Relation("input", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Disjunction(Seq(
            Call("input", Seq(Var("n$0"))),
            Eq(Var("n$0"), IntNum(1), true),
            Eq(Var("n"), Sub(Var("n$0"), IntNum(1)))
          ), Seq(
            Eq(Var("n"), IntNum(3))
          ))
        ))
      )),
      Relation("fac", Seq(
        Param("n", TInt),
        Param("r", TInt)
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("n"))),
          Disjunction(Seq(
            Eq(Var("n"), IntNum(1)),
            Eq(Var("r"), IntNum(1))
          ), Seq(
            Eq(Var("n"), IntNum(1), true),
            Call("fac", Seq(Sub(Var("n"), IntNum(1)), Var("r$0"))),
            Eq(Var("r"), Mul(Var("n"), Var("r$0")))
          ))
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    val mainRel = res("fac")
    assert(mainRel.size == 3)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(1, 1)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(2, 2)))
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(3, 6)))
  }

  test("Disjunction - Recursive") {
    val mod = Module("Test3", BaseIR.language + stringIR + disjunctionIR + demandIR + arithIR + dataIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("count", Seq(
        Param("x", TDemand(TData("TList"))),
        Param("prevC", TDemand(TString)),
        Param("c", TString)
      ), Seq(
        Body(Seq(
          Disjunction(
            Seq(
              Deconstruct(Var("x"), "TNil", Seq()),
              Eq(Var("c"), StringConcat(Var("prevC"), StringLit("N")))
            ),
            Seq(
              Deconstruct(Var("x"), "TCons", Seq(Var("hd"), Var("tail"))),
              Eq(Var("tmp"), StringConcat(Var("prevC"), StringLit("C"))),
              Call("count", Seq(Var("tail"), Var("tmp"), Var("c")))
            )
          )
        )),
      )),

      Relation("main", Seq(
        Param("c", TString),
      ), Seq(
        Body(Seq(
          Eq(Var("z"), Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))),
          Call("count", Seq(Var("z"), StringLit(""), Var("c")))
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == "CN")
  }

  /* Block */

  test("Block - Factorial") {
    val mod = Module("Test3", BaseIR.language + arithIR + blockIR, Seq(
      ExtensionalRelation("ext_main$input", Seq(Param("n", TInt))),
      Relation("fact", Seq(
        Param("n", TInt),
        Param("fact_result$0", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("fact_result$0"), Block(Seq(
            Call("fact$input", Seq(Var("n"))),
            Eq(Var("n"), IntNum(1))
          ), IntNum(1))
        ))),
        Body(Seq(
          Eq(Var("fact_result$0"), Block(Seq(
            Call("fact$input", Seq(Var("n"))),
            Eq(Var("n"), IntNum(1), true),
            Call("fact", Seq(Sub(Var("n"), IntNum(1)), Var("fact_call$0"))),
          ), Mul(Var("n"), Var("fact_call$0"))))
        ))),
      ),
      Relation("main", Seq(
        Param("n", TInt),
        Param("main_result$0", TInt)
      ), Seq(
        Body(Seq(
          ExtensionalCall("ext_main$input", Seq(Var("n"))),
          Call("fact", Seq(Var("n"), Var("main_result$0")))
        )),
      )).addHint(MainHint),
      Relation("fact$input", Seq(
        Param("n$0", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("n$0"), Block(Seq(
            Call("fact$input", Seq(Var("n"))),
            Eq(Var("n"), IntNum(1), true),
          ), Sub(Var("n"), IntNum(1))))
        )),
        Body(Seq(
          ExtensionalCall("ext_main$input", Seq(Var("n$0"))),
        ))
      ))
    ))

    val res = interp(mod, Seq(execution.Relation1("ext_main$input", Seq("param_0"), Seq(Seq(5)))))
    val mainRel = res("main")
    assert(mainRel.size == 1)
    assert(mainRel.entries.map(mainRel.flattenEntry).toSet.contains(Seq(5, 120)))
  }

  /* Datamatch */

  test("Datamatch - Recursive") {
    val mod = Module("Test3", BaseIR.language + stringIR + datamatchIR + demandIR + arithIR + dataIR, Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TData("TList")),
      CaseDefinition("TCons", Seq(TInt, TData("TList")), TData("TList")),

      Relation("count", Seq(
        Param("x", TDemand(TData("TList"))),
        Param("prevC", TDemand(TString)),
        Param("c", TString)
      ), Seq(
        Body(Seq(
          Match(Var("x"), Seq(
            Case(RefByName("TNil"), Seq(), Seq(
              Eq(Var("c"), StringConcat(Var("prevC"), StringLit("N")))
            )),
            Case(RefByName("TCons"), Seq(Var("hd"), Var("tail")), Seq(
              Eq(Var("tmp"), StringConcat(Var("prevC"), StringLit("C"))),
              Call("count", Seq(Var("tail"), Var("tmp"), Var("c")))
            )),
          ))
        )),
      )),

      Relation("main", Seq(
        Param("c", TString),
      ), Seq(
        Body(Seq(
          Eq(Var("z"), Construct("TCons", Seq(IntNum(1), Construct("TNil", Seq())))),
          Call("count", Seq(Var("z"), StringLit(""), Var("c")))
        )),
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == "CN")
  }

  /* Set */

  test("Set - Literal") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
      Relation("main", Seq(
        Param("out", TSet(TInt))
      ), Seq(
        Body(Seq(
          Eq(Var("out"), SetLit(Seq(IntNum(1), IntNum(2))))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Set(1,2))
  }

  test("Set - Union") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
      Relation("main", Seq(
        Param("out", TSet(TInt))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2)))),
          Eq(Var("y"), SetLit(Seq(IntNum(2), IntNum(3)))),
          Eq(Var("out"), SetUnion(Var("x"), Var("y")))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Set(1, 2, 3))
  }

  test("Set - Intersect") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
      Relation("main", Seq(
        Param("out", TSet(TInt))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2)))),
          Eq(Var("y"), SetLit(Seq(IntNum(2), IntNum(3)))),
          Eq(Var("out"), SetIntersection(Var("x"), Var("y")))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Set(2))
  }

  test("Set - Member (binding)") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
      Relation("main", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
          SetMember(Var("out"), Var("x"))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRes = res("main")
    assert(mainRes.size == 5)
    assert(mainRes.toSet == Set(1, 2, 3, 4, 5))
  }

  test("Set - Member (bound)") {
    val mod = Module("Test1", BaseIR.language + boolIR + arithIR + setIR, Seq(
      Relation("main", Seq(
        Param("out", TBoolean)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
          SetMember(IntNum(1), Var("x")),
          Eq(BoolTrue, Var("out"))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRes = res("main")
    assert(mainRes.size == 1)
    assert(mainRes.entries.head == true)
  }

  test("Set - Comprehension") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
      Relation("main", Seq(
        Param("out", TSet(TInt))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
          Eq(Var("out"), SetComprehension(Var("x$i"), Seq(
            SetMember(Var("x$i"), Var("x")),
            LT(Var("x$i"), IntNum(3))
          )))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRes = res("main")
    assert(mainRes.size == 1)
    assert(mainRes.entries.head == Set(1, 2))
  }

  test("Set - Comprehension (Empty)") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
      Relation("main", Seq(
        Param("out", TSet(TInt))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetLit(Seq(IntNum(1), IntNum(2), IntNum(3), IntNum(4), IntNum(5)))),
          Eq(Var("out"), SetComprehension(Var("x$i"), Seq(
            SetMember(Var("x$i"), Var("x")),
            LT(Var("x$i"), IntNum(0))
          )))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    val mainRes = res("main")
    assert(mainRes.size == 1)
    assert(mainRes.entries.head == Set())
  }

  test("Set - SetFrom") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
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
        Param("x", TSet(TInt))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetFrom(RefByName("nums")))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Set(1, 2))
  }

  test("Set - SetFrom (Tuple)") {
    val mod = Module("Test1", BaseIR.language + arithIR + setIR, Seq(
      Relation("nums", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(3)),
          Eq(Var("y"), IntNum(4))
        ))
      )),
      Relation("main", Seq(
        Param("x", TSet(TTuple(Seq(TInt, TInt))))
      ), Seq(
        Body(Seq(
          Eq(Var("x"), SetFrom(RefByName("nums")))
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Set(Seq(1, 2), Seq(3, 4)))
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
    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Map())
}

  test("Map - Literal") {
    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
      Relation(
        "main",
        Seq(Param("m", TMap(TString, TInt))),
        Seq(Body(Seq(
          Eq(Var("m"), MapLit.from((StringLit("A"), IntNum(1)), (StringLit("B"), IntNum(2))))
        )))
      )
    ))
    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Map(
      "A" -> Set(1),
      "B" -> Set(2)
    ))
  }

  test("Map - Union") {
    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
      Relation(
        "main",
        Seq(Param("m2", TMap(TString, TInt))),
        Seq(Body(Seq(
          Eq(Var("m1"), MapLit.from((StringLit("A"), IntNum(1)))),
          Eq(Var("m2"), MapUnion(Var("m1"), MapLit.from((StringLit("B"), IntNum(2)))))
        )))
      )
    ))
    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Map(
      "A" -> Set(1),
      "B" -> Set(2)
    ))
  }

  test("Map - Union (Collision)") {
    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
      Relation(
        "main",
        Seq(Param("m2", TMap(TString, TInt))),
        Seq(Body(Seq(
          Eq(Var("m1"), MapLit.from((StringLit("A"), IntNum(1)))),
          Eq(Var("m2"), MapUnion(Var("m1"), MapLit.from((StringLit("A"), IntNum(2)))))
        )))
      )
    ))
    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Map(
      "A" -> Set(1, 2),
    ))
  }

  test("Map - From Relation") {
    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
      Relation(
        "main",
        Seq(Param("m", TMap(TString, TInt))),
        Seq(Body(Seq(
          Eq(Var("m"), MapFrom("someCall"))
        )))
      ),
      Relation(
        "someCall",
        Seq(
          Param("k", TDemand(TString)),
          Param("v", TInt)
        ),
        Seq(
          Body(Seq(
            Eq(Var("k"), StringLit("A")),
            Eq(Var("v"), IntNum(1))
          )),
          Body(Seq(
            Eq(Var("k"), StringLit("B")),
            Eq(Var("v"), IntNum(2))
          ))
        )
      )
    ))
    val res = interp(mod)
    assert(res("main").size == 1)
    val mapFun = res("main").entries.head.asInstanceOf[Function[Any, Any]]
    assert(mapFun("A") == Set(1))
    assert(mapFun("B") == Set(2))
    /*assert(res("main").entries.head == Map(
      "A" -> Set(1),
      "B" -> Set(2)
    ))*/
  }

  test("Map - Comprehension") {
    val mod = Module("Test1", BaseIR.language + arithIR + mapIR, Seq(
      Relation(
        "main",
        Seq(Param("m2", TMap(TInt, TInt))),
        Seq(Body(Seq(
          Eq(Var("m1"), MapLit(Seq(
            IntNum(1) -> IntNum(2),
            IntNum(2) -> IntNum(3)
          ))),
          Eq(Var("m2"), MapComprehension(Var("k"), Add(Var("v"), IntNum(1)), Seq(
            MapContains(Var("m1"), Var("k")),
            Eq(Var("v"), MapLookUp(Var("m1"), Var("k")))
          )))
        )))
      )
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Map(
      1 -> Set(3),
      2 -> Set(4)
    ))
  }

  test("Map - plus (collision)") {
    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
      Relation(
        "main",
        Seq(Param("m2", TMap(TString, TInt))),
        Seq(Body(Seq(
          Eq(Var("m1"), MapLit.from((StringLit("A"), IntNum(1)))),
          Eq(Var("m2"), MapPlus(Var("m1"), StringLit("A"), IntNum(2)))
        )))
      )
    ))
    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Map(
      "A" -> Set(2),
    ))
  }

  test("Map - plus (No collision)") {
    val mod = Module("Test1", BaseIR.language + arithIR + stringIR + mapIR, Seq(
      Relation(
        "main",
        Seq(Param("m2", TMap(TString, TInt))),
        Seq(Body(Seq(
          Eq(Var("m1"), MapLit.from((StringLit("A"), IntNum(1)))),
          Eq(Var("m2"), MapPlus(Var("m1"), StringLit("B"), IntNum(2)))
        )))
      )
    ))
    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == Map(
      "A" -> Set(1),
      "B" -> Set(2),
    ))
  }

  test("Map - Fun") {
    val mod = Module("Test1", BaseIR.language + arithIR + mapIR, Seq(
      Relation("main",
        Seq(Param("x", TInt)),
        Seq(Body(Seq(
          Eq(Var("map1"), MapLit(Seq(IntNum(1) -> IntNum(2), IntNum(3) -> IntNum(4)))),
          Eq(Var("map2"), MapFun(Seq(Param("key", TInt)), MapLookUp(Var("map1"), Var("key")))),
          Eq(Var("x"), MapLookUp(Var("map2"), IntNum(1)))
        )))
      )
    ))
    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == 2)
  }

  /*test("Map - Fun (Map return)") {
    val mod = Module("Test1", BaseIR.language + arithIR + mapIR, Seq(
      Relation("main",
        Seq(Param("m", TMap(TInt, TInt))),
        Seq(Body(Seq(
          Eq(Var("map1"), MapLit(Seq(IntNum(1) -> IntNum(2), IntNum(3) -> IntNum(4)))),
          Eq(Var("m"), MapFun(Seq(Param("key", TInt)), MapLookUp(Var("map1"), Var("key")))),
          Eq(Var("x"), MapLookUp(Var("m"), IntNum(1)))
        )))
      )
    ))
    val res = interp(mod)
    println(res("main").asTable)
    assert(res("main").size == 1)
    assert(res("main").entries.head == 2)
  }*/
  
  /* Impure */

  object SimpleCounterImpurityKind extends ImpurityKind:
    override val name: String = "MonoImpurity"
    override val ty: Type = TInt

  test("Impure: Single relation") {
    val mod = Module("Test1", BaseIR.language + arithIR + impureIR, Seq(
      Relation("main", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Impure.init(IntNum(0), SimpleCounterImpurityKind),
          Impure.counter(Name("counter"), Eq(Var("out"), Var("counter")), SimpleCounterImpurityKind)
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 1)
    assert(res("main").entries.head == 0)
  }

  test("Impure: Unbalanced counter") {
    val mod = Module("Test1", BaseIR.language + arithIR + impureIR, Seq(
      Relation("helper", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          // out == 0
          Impure.counter(Name("counter"), Eq(Var("out"), Var("counter")), SimpleCounterImpurityKind)
        )),
        Body(Seq(
          // out == 1
          Impure.counter(Name("_$"), Seq(), SimpleCounterImpurityKind), // inc the impurity counter by one
          Impure.counter(Name("counter"), Eq(Var("out"), Var("counter")), SimpleCounterImpurityKind)
        ))
      )),
      Relation("main", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Impure.init(IntNum(0), SimpleCounterImpurityKind),
          Call("helper", Seq(Var("out"))),
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    assert(res("main").size == 2)
    assert(res("main").entries.toSet == Set(0,1))
  }

  // This is expected to not terminate. That's why it is commented out, but you comment it in to debug.
  /*test("Impure: Recursion") {
    val mod = Module("Test1", BaseIR.language + arithIR + impureIR, Seq(
      Relation("helper", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          // out == 0
          Impure.counter(Name("counter"), Eq(Var("out"), Var("counter")), SimpleCounterImpurityKind)
        )),
        Body(Seq(
          Call("helper", Seq(Var("out"))),
          // Increase counter to cause an endless-loop
          Impure.counter(Name("_$"), Seq(), SimpleCounterImpurityKind)
        ))
      )),
      Relation("main", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Impure.init(IntNum(0), SimpleCounterImpurityKind),
          Call("helper", Seq(Var("out"))),
        ))
      )).addHint(MainHint)
    ))

    val res = interp(mod)
    println(res("main").toSet)
    assert(res("main").size == 2)
    assert(res("main").entries.toSet == Set(0, 1))
  }*/