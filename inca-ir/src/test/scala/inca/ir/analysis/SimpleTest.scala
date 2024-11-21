package inca.ir.analysis

import inca.ir.execution.interpreter.Executor
import inca.ir.{BaseIR, Body, Call, CompiledTestUnit, Eq, Module, Param, Relation, Var, execution, string2name, term2Arg, termList2ArgList}
import inca.ir.extension.arithmetic.{Add, IntNum, Mul, TInt, IR as arithIR}
import inca.ir.extension.impure.MainHint
import inca.ir.typing.IRTypechecker
import org.scalatest.funsuite.AnyFunSuiteLike


class SimpleTest extends AnyFunSuiteLike:

  def interp(mod: Module): Seq[execution.Relation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val interp = new Executor
    val compiled = CompiledTestUnit(mod)
    val engine = interp.instantiate(compiled)
    val res = engine.readAll()
    res.foreach { r =>
      println(r.asTable)
    }
    res

  test("Single relation") {
    val mod = Module("Test1", BaseIR.language + arithIR, Seq(
      Relation("main", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Mul(Add(IntNum(3), IntNum(4)), IntNum(2)))
        ))
      )).addHint(MainHint)
    ))

    interp(mod)
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

    interp(mod)
  }

  test("Recursion") {
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

    interp(mod)
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
        ))
      )).addHint(MainHint),
    ))

    interp(mod)
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

    interp(mod)
  }
