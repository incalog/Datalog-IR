package inca.frontend.functional.integration

import inca.frontend.functional.executor.FunctionalExecutor._
import inca.util.FileUtil
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionsDataTest extends AnyFunSuite {

  ignore("Plus Example") {
    val code = FileUtil.readFile("functional/unittests/PlusReal.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq(q"Succ(Succ(Zero()))", q"Succ(Zero())"))
      == fun.result(q"Succ(Succ(Succ(Zero())))"))
    assert(fun.execute("main", Seq(q"Succ(Succ(Succ(Succ(Zero()))))", q"Succ(Succ(Zero()))"))
      == fun.result(q"Succ(Succ(Succ(Succ(Succ(Succ(Zero()))))))"))
  }

  ignore("graph example with functions as predicates") {
    val code = FileUtil.readFile("functional/unittests/BusStation.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(q"""BusStation("B", 5)"""))
  }

  test("Binary tree example") {
    val code = FileUtil.readFile("functional/unittests/BinaryTree.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(q"20"))
  }

  // TODO: Remove me
  test("Datatypes") {
    val code = FileUtil.readFile("functional/unittests/Datatypes2.finca")
    val fun = loadFunction(code)
    //fun.execute("main", Seq())
    //fun.printAllMatches()
    //assert(fun.execute("main", Seq()) == fun.result(q"20"))
  }

  test("Simple Fold Int Example") {
    val code = FileUtil.readFile("functional/unittests/FoldInt.finca")
    val fun = loadFunction(code)
    assert(fun.execute("sum", Seq(q"1", q"10")) == fun.result(q"55"))
  }

  test("Simple Fold Int Projected Example") {
    val code = FileUtil.readFile("functional/unittests/FoldIntProjected.finca")
    val fun = loadFunction(code)
    assert(fun.execute("sum", Seq(q"1", q"10")) == fun.result(q"110"))
  }

  ignore("Simple Fold Example") {
    // TODO: This test will fail, because we compare a MockURI with a truediff tree
    val code = FileUtil.readFile("functional/unittests/FoldADT.finca")
    val fun = loadFunction(code)
    assert(fun.execute("sum", Seq(q"1", q"10")) == fun.result(q"V(55)"))
  }

  ignore("Type Checker Example") {
    val code = FileUtil.readFile("functional/lambdacalculus/LambdaCalculus.finca")
    val fun = loadFunction(code)
    assert(fun.execute("mainTypeOf", Seq(q"TNum(1)"))
      == fun.result(q"SomeType(TInt())"))
    assert(fun.execute("mainTypeOf", Seq(q"""TLam("x", TInt(), TVar("x"))"""))
      == fun.result(q"SomeType(TFun(TInt(), TInt()))"))
    assert(fun.execute("mainTypeOf", Seq(q"""TLam("x", TInt(), TVar("y"))"""))
      == fun.result(q"NoType()"))
    assert(fun.execute("mainTypeOf", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""))
      == fun.result(q"SomeType(TInt())"))
    assert(fun.execute("mainTypeOf", Seq(q"""TApp(TNum(12), TNum(11))"""))
      == fun.result(q"NoType()"))
  }

  ignore("Type Erasure Example") {
    val code = FileUtil.readFile("functional/lambdacalculus/LambdaCalculus.finca")
    val fun = loadFunction(code)
    assert(fun.execute("erase", Seq(q"TNum(1)"), deleteInput = true)
      == fun.result(q"Num(1)"))
    assert(fun.execute("erase", Seq(q"""TLam("x", TInt(), TVar("x"))"""), deleteInput = true)
      == fun.result(q"""Lam("x", Var("x"))"""))
    assert(fun.execute("erase", Seq(q"""TLam("x", TInt(), TVar("y"))"""), deleteInput = true)
      == fun.result(q"""Lam("x", Var("y"))"""))
    assert(fun.execute("erase", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""), deleteInput = true)
      == fun.result(q"""App(Lam("x", Var("x")), Num(1337))"""))
    assert(fun.execute("erase", Seq(q"""TApp(TNum(12), TNum(11))"""), deleteInput = true)
      == fun.result(q"App(Num(12), Num(11))"))
  }

  ignore("Interpreter Example") {
    val code = FileUtil.readFile("functional/lambdacalculus/LambdaCalculus.finca")
    val fun = loadFunction(code)
    assert(fun.execute("mainInterp", Seq(q"Num(1)"), deleteInput = true)
      == fun.result(q"SomeVal(VNum(1))"))
    assert(fun.execute("mainInterp", Seq(q"""Lam("x", Var("x"))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VClosure("x", Var("x"), EmptyEnv()))"""))
    assert(fun.execute("mainInterp", Seq(q"""Lam("x", Var("y"))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VClosure("x", Var("y"), EmptyEnv()))"""))
    assert(fun.execute("mainInterp", Seq(q"""App(Lam("y", Lam("x", Var("y"))), Num(1))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VClosure("x", Var("y"), BindEnv("y", VNum(1), EmptyEnv())))"""))
    assert(fun.execute("mainInterp", Seq(q"""App(Lam("x", Var("y")), Num(1))"""), deleteInput = true)
      == fun.result(q"""NoVal()"""))
    assert(fun.execute("mainInterp", Seq(q"""App(Lam("x", Var("x")), Num(1337))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VNum(1337))"""))
    assert(fun.execute("mainInterp", Seq(q"""App(Num(12), Num(11))"""), deleteInput = true)
      == fun.result(q"NoVal()"))
  }

  test("Checking+Erasure+Interpreting Example") {
    val code = FileUtil.readFile("functional/lambdacalculus/LambdaCalculus.finca")
    val fun = loadFunction(code)

    // type of peano = (a -> a) -> (a -> a)
    val zero = q"""TLam("f", TFun(TInt(), TInt()), TLam("x", TInt(), TVar("x")))"""
    val one = q"""TLam("f", TFun(TInt(), TInt()), TLam("x", TInt(), TApp(TVar("f"), TVar("x"))))"""
    val two = q"""TLam("f", TFun(TInt(), TInt()), TLam("x", TInt(), TApp(TVar("f"), TApp(TVar("f"), TVar("x")))))"""
    val three = q"""TLam("f", TFun(TInt(), TInt()), TLam("x", TInt(), TApp(TVar("f"), TApp(TVar("f"), TApp(TVar("f"), TVar("x"))))))"""

    val succ =
      q"""
          TLam("n", TFun(TFun(TInt(), TInt()), TFun(TInt(), TInt())),
            TLam("f", TFun(TInt(), TInt()),
              TLam("x", TInt(),
                TApp(TVar("f"), TApp(TApp(TVar("n"), TVar("f")), TVar("x"))))))
        """

    val plus =
      q"""
          TLam("m", TFun(TFun(TInt(), TInt()), TFun(TInt(), TInt())),
            TLam("n", TFun(TFun(TInt(), TInt()), TFun(TInt(), TInt())),
              TLam("f", TFun(TInt(), TInt()),
                TLam("x", TInt(),
                  TApp(
                    TApp(TVar("m"), TVar("f")),
                    TApp(TApp(TVar("n"), TVar("f")), TVar("x"))
                  )))))

       """
    // TODO how to assert result?
    println(fun.execute("main", Seq(three)))
    println(fun.execute("main", Seq(q"TApp($succ, $three)")))
    println(fun.execute("main", Seq(q"TApp(TApp($plus, TApp($succ, $three)), $one)")))
  }


  test("type cast of ADT") {
    val code = FileUtil.readFile("functional/unittests/TypeCast.finca")
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq(q"Zero()")).res.nonEmpty)
    assert(fun.execute("main", Seq(q"Succ(Succ(Zero()))")).res.nonEmpty)
    assert(fun.execute("main", Seq(q"1")).res.isEmpty)
    assert(fun.execute("main", Seq(q"True()")).res.isEmpty)
  }

  test("Accessing Parent of ADT") {
    val code = FileUtil.readFile("functional/unittests/ParentAccess.finca")
    val fun = loadFunction(code)
    val res1 = fun.execute("main", Seq())

    val res2 = fun.execute("main2", Seq(q"Succ(Succ(Zero()))"))
    // fun.printResult(res2)
    val res3 = fun.execute("main2", Seq(q"Zero()"))
    // fun.printResult(res3)
  }
}
