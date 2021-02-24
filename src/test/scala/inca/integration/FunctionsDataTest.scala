package inca.integration

import inca.Executor._
import inca.examples.{Code, LambdaCalculus}
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionsDataTest extends AnyFunSuite {

  test("Plus Example") {
    val fun = loadFunction(Code.plusRealModule)
    assert(fun.execute("main_bbf", Seq(q"Succ(Succ(Zero()))", q"Succ(Zero())"))
      == fun.result(q"Succ(Succ(Succ(Zero())))"))
    assert(fun.execute("main_bbf", Seq(q"Succ(Succ(Succ(Succ(Zero()))))", q"Succ(Succ(Zero()))"))
      == fun.result(q"Succ(Succ(Succ(Succ(Succ(Succ(Zero()))))))"))
    fun.printAllMatches()
  }

  test("Type Checker Example") {
    val fun = loadFunction(LambdaCalculus.typeOfModule)
    assert(fun.execute("main_bf", Seq(q"TNum(1)"), deleteInput = true)
      == fun.result(q"SomeType(TInt())"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("x"))"""), deleteInput = true)
      == fun.result(q"SomeType(TFun(TInt(), TInt()))"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("y"))"""), deleteInput = true)
      == fun.result(q"NoType()"))
    assert(fun.execute("main_bf", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""), deleteInput = true)
      == fun.result(q"SomeType(TInt())"))
    assert(fun.execute("main_bf", Seq(q"""TApp(TNum(12), TNum(11))"""), deleteInput = true)
      == fun.result(q"NoType()"))
    fun.printAllMatches()
  }

  test("Type Checker Relation Example") {
    val fun = loadFunction(LambdaCalculus.typeOfRelModule)
    assert(fun.execute("main_bf", Seq(q"TNum(1)"), deleteInput = true)
      == fun.result(q"TInt()"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("x"))"""), deleteInput = true)
      == fun.result(q"TFun(TInt(), TInt())"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("y"))"""), deleteInput = true)
      == fun.results(Seq()))
    assert(fun.execute("main_bf", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""), deleteInput = true)
      == fun.result(q"TInt()"))
    assert(fun.execute("main_bf", Seq(q"""TApp(TNum(12), TNum(11))"""), deleteInput = true)
      == fun.results(Seq()))
    fun.printAllMatches()
  }

  test("Type Erasure Example") {
    val fun = loadFunction(LambdaCalculus.eraseModule)
    assert(fun.execute("main_bf", Seq(q"TNum(1)"), deleteInput = true)
      == fun.result(q"Num(1)"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("x"))"""), deleteInput = true)
      == fun.result(q"""Lam("x", Var("x"))"""))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("y"))"""), deleteInput = true)
      == fun.result(q"""Lam("x", Var("y"))"""))
    assert(fun.execute("main_bf", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""), deleteInput = true)
      == fun.result(q"""App(Lam("x", Var("x")), Num(1337))"""))
    assert(fun.execute("main_bf", Seq(q"""TApp(TNum(12), TNum(11))"""), deleteInput = true)
      == fun.result(q"App(Num(12), Num(11))"))
    fun.printAllMatches()
  }

  test("Interpreter Example") {
    val fun = loadFunction(LambdaCalculus.interpModule)
    assert(fun.execute("main_bf", Seq(q"Num(1)"), deleteInput = true)
      == fun.result(q"SomeVal(VNum(1))"))
    assert(fun.execute("main_bf", Seq(q"""Lam("x", Var("x"))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VClosure("x", Var("x"), EmptyEnv()))"""))
    assert(fun.execute("main_bf", Seq(q"""Lam("x", Var("y"))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VClosure("x", Var("y"), EmptyEnv()))"""))
    assert(fun.execute("main_bf", Seq(q"""App(Lam("y", Lam("x", Var("y"))), Num(1))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VClosure("x", Var("y"), BindEnv("y", VNum(1), EmptyEnv())))"""))
    assert(fun.execute("main_bf", Seq(q"""App(Lam("x", Var("y")), Num(1))"""), deleteInput = true)
      == fun.result(q"""NoVal()"""))
    assert(fun.execute("main_bf", Seq(q"""App(Lam("x", Var("x")), Num(1337))"""), deleteInput = true)
      == fun.result(q"""SomeVal(VNum(1337))"""))
    assert(fun.execute("main_bf", Seq(q"""App(Num(12), Num(11))"""), deleteInput = true)
      == fun.result(q"NoVal()"))
    fun.printAllMatches()
  }

  test("Checking+Erasure+Interpreting Example") {
    val fun = loadFunction(LambdaCalculus.completeLCModule)
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
    assert(fun.execute("main_bf", Seq(three))
      == fun.result(q"""SomeVal(VClosure("f", Lam("x", App(Var("f"), App(Var("f"), App(Var("f"), Var("x"))))), EmptyEnv()))"""))

    // TODO how to assert result?
    fun.execute("main_bf", Seq(q"TApp($succ, $three)"))
    fun.printMatches("main_bf")
    fun.execute("main_bf", Seq(q"TApp(TApp($plus, TApp($succ, $three)), $one)"))
    fun.printMatches("main_bf")
  }
}
