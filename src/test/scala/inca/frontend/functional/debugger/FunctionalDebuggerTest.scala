package inca.frontend.functional.debugger

import inca.compiler.Compiler
import inca.examples.functional.{ADT, Code}
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import meta.quasiquotes._

class FunctionalDebuggerTest extends AnyFunSuite {

  def initDebugger(module: CompiledFunctionalModule): FunctionalDebugger = {
    new FunctionalDebugger(module)
  }

  def compile(code: String): CompiledFunctionalModule =
    Compiler.compileFunctional(code, FunctionalOptions().withOptimizations(Seq()))

  def assertControlTraceSize(prog: String, main: String, args: meta.Term*)(expected: Int): Assertion = {
    val compiledExample = compile(prog)
    val debugger = initDebugger(compiledExample)
    debugger.entry(main, args: _*)
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    println()
    assertResult(expected)(debugger.controlTraceFrontend.size)
  }

  test("nested let") {
    assertControlTraceSize(Code.varExample, "main")(6)
  }

  test("simple function call") {
    assertControlTraceSize(Code.incModule, "main")(6)
  }

  test("if example") {
    assertControlTraceSize(Code.ifExample, "main")(4)
  }

  test("if example 2") {
    assertControlTraceSize(Code.ifExample2, "main")(8)
  }

  def ifControlJump(b1: Boolean, b2: Boolean): String =
    s"""module M
       |@main def main(): Int =
       |  if ($b1 == true)
       |    if ($b2 == true)
       |      0 + 0
       |    else
       |      1 + 0
       |  else
       |    if ($b2 == true)
       |      2 + 0
       |    else
       |      3 + 0
       |""".stripMargin

  test("if control jumping") {
    for (b1 <- Seq(true, false); b2 <- Seq(true, false)) {
      assertControlTraceSize(ifControlJump(b1, b2), "main")(5)
    }
  }

  test("fib example") {
    assertControlTraceSize(Code.fibModule, "main", q"3")(28)
  }

  test("Constructor calls example") {
    val code = Code.module(
      ADT.Nat_code,
      """@main def main(): Nat = Succ(Succ(Zero()))
        |""".stripMargin
    )
    assertControlTraceSize(code, "main")(5)
  }


  val matchProg: String =
    s"""module M
       |data Exp = Var(String) | Num(Int) | Add(Exp, Exp) | Let(String, Exp, Exp)
       |@main def main(exp: Exp): Int = exp match {
       |  case Var(x) => 1
       |  case Num(i) => 2
       |  case Add(l, r) => 3
       |  case Let(n, bound, body) => 4
       |}
       |""".stripMargin

  test("pattern matching multiple constructors") {
    assertControlTraceSize(matchProg, "main", q"""Var("x")""")(3)
    assertControlTraceSize(matchProg, "main", q"""Num(1)""")(4)
    assertControlTraceSize(matchProg, "main", q"""Add(Var("y"), Num(2))""")(5)
    assertControlTraceSize(matchProg, "main", q"""Let("x", Num(3), Add(Var("x"), Num(2)))""")(6)
  }


  val nestedMatchProg: String =
    s"""module M
       |data Exp = Var(String) | Num(Int) | Add(Exp, Exp) | Let(String, Exp, Exp)
       |@main def main(exp1: Exp, exp2: Exp): Boolean = exp1 match {
       |  case Var(x1) => exp2 match {
       |    case Var(x2) => true
       |    case Num(i2) => false
       |    case Add(l2, r2) => false
       |    case Let(n2, bound2, body2) => false
       |  }
       |  case Num(i1) => exp2 match {
       |    case Var(x2) => false
       |    case Num(i2) => true
       |    case Add(l2, r2) => false
       |    case Let(n2, bound2, body2) => false
       |  }
       |  case Add(l1, r1) => exp2 match {
       |    case Var(x2) => false
       |    case Num(i2) => false
       |    case Add(l2, r2) => true
       |    case Let(n2, bound2, body2) => false
       |    }
       |  case Let(n1, bound1, body1) => exp2 match {
       |    case Var(x2) => false
       |    case Num(i2) => false
       |    case Add(l2, r2) => false
       |    case Let(n2, bound2, body2) => true
       |  }
       |}
       |""".stripMargin

  // TODO fix
  test("nested pattern matching") {
    val (v, n, a, l) = (q"""Var("x")""", q"Num(1)", q"Add(Num(1), Num(2))", q"""Let("x", Num(1), Num(2))""")

    assertControlTraceSize(nestedMatchProg, "main", v, v)(4)
    assertControlTraceSize(nestedMatchProg, "main", v, n)(5)
    assertControlTraceSize(nestedMatchProg, "main", v, a)(6)
    assertControlTraceSize(nestedMatchProg, "main", v, l)(7)
    assertControlTraceSize(nestedMatchProg, "main", n, v)(5)
    assertControlTraceSize(nestedMatchProg, "main", n, n)(6)
    assertControlTraceSize(nestedMatchProg, "main", n, a)(7)
    assertControlTraceSize(nestedMatchProg, "main", n, l)(8)
    assertControlTraceSize(nestedMatchProg, "main", a, v)(6)
    assertControlTraceSize(nestedMatchProg, "main", a, n)(7)
    assertControlTraceSize(nestedMatchProg, "main", a, a)(8)
    assertControlTraceSize(nestedMatchProg, "main", a, l)(9)
    assertControlTraceSize(nestedMatchProg, "main", l, v)(7)
    assertControlTraceSize(nestedMatchProg, "main", l, n)(8)
    assertControlTraceSize(nestedMatchProg, "main", l, a)(9)
    assertControlTraceSize(nestedMatchProg, "main", l, l)(10)
  }

  test("plus example") {
    assertControlTraceSize(Code.plusRealModule, "main", q"Succ(Succ(Zero()))", q"Succ(Zero())")(18)
  }

  // TODO fix
  test("plus example extra") {
    assertControlTraceSize(Code.plusRealModuleExtra, "main", q"Succ(Succ(Zero()))", q"Succ(Zero())")(21)
  }
}
