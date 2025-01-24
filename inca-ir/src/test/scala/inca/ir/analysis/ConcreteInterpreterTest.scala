package inca.ir.analysis

import inca.ir.execution.interpreter.{Executor, InterpreterRelation}
import inca.ir.extension.arithmetic.analysis.interpreter.CIntV
import inca.ir.{Arg, BaseIR, Body, Call, CompiledTestUnit, Eq, ExtensionalCall, ExtensionalRelation, MainHint, Module, Name, Param, Relation, Var, WildcardArg, execution, string2name, term2Arg, termList2ArgList}
import inca.ir.extension.arithmetic.{Add, ArithmeticAggregationOperator, IntNum, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
import inca.ir.typing.IRTypechecker
import org.scalatest.funsuite.AnyFunSuiteLike
import sturdy.fix.Fixpoint


class ConcreteInterpreterTest extends AnyFunSuiteLike:

  def interp(mod: Module, edb: Seq[execution.Relation] = Seq()): Map[String, execution.Relation] =
    println(mod)

    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val interp = new Executor
    val compiled = CompiledTestUnit(mod)
    val engine = interp.instantiate(compiled)
    edb.foreach(engine.insert)
    val res = engine.readAll()
    res.foreach { r =>
      println(r.asTable)
    }
    res.map(r => r.name -> r).toMap

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
    Fixpoint.DEBUG = true
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
