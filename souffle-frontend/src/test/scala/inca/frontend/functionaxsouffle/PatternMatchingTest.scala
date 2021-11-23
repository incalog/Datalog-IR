package inca.frontend.functionaxsouffle

import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class PatternMatchingTest extends AnyFunSuite {
  val souffleCode = ""

  test("constructor without arguments") {
    val code =
      s"""module PrimitiveTuple
         |data Bool = True() | False()
         |
         |@main def main(x: Bool): Int =
         |  x match {
         |   case True() => 1
         |   case False() => -1
         |}
         |""".stripMargin
    val fun = FunctionalXSouffleExecutor.loadFunction(code, souffleCode)
    println(fun.compiled.ir)
    assert(fun.execute("main", Seq(q"True()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(1))
    assert(fun.execute("main", Seq(q"False()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(-1))
  }

  test("constructor with arguments") {
    val code =
      s"""module PrimitiveTuple
         |data Nat = Zero() | Succ(Nat)
         |
         |@main def main(x: Nat): Int = x match {
         |   case Succ(p) => main(p) + 1
         |   case Zero() => 0
         |}
         |""".stripMargin
    val fun = FunctionalXSouffleExecutor.loadFunction(code, souffleCode)
    assert(fun.execute("main", Seq(q"Zero()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(0))
    assert(fun.execute("main", Seq(q"Succ(Zero())"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(1))
    assert(fun.execute("main", Seq(q"Succ(Succ(Zero()))"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(2))
  }

  test("wilcard pattern for constructors") {
    val code =
      s"""module PrimitiveTuple
         |data Nat = Zero() | Succ(Nat)
         |
         |@main def main(x: Nat): Int = x match {
         |   case Succ(p) => main(p) + 1
         |   case x => 0
         |}
         |""".stripMargin
    val fun = FunctionalXSouffleExecutor.loadFunction(code, souffleCode)
    assert(fun.execute("main", Seq(q"Zero()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(0))
    assert(fun.execute("main", Seq(q"Succ(Zero())"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(1))
    assert(fun.execute("main", Seq(q"Succ(Succ(Zero()))"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(2))
  }

  test("primitive tuple") {
    val code =
      s"""module PrimitiveTuple
         |@main def main(): Int =
         |  let x = (1, true) in
         |    x match {
         |      case (x1,x2) => x1
         |    }
         |""".stripMargin
    val fun = FunctionalXSouffleExecutor.loadFunction(code, souffleCode)
    assert(fun.execute("main", Seq[meta.Term](), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(1))
  }

  test("Tuple containing construstors without arguments") {
    val code =
      s"""module DataTuple
         |data Bool = True() | False()
         |@main def main(x: Bool, y: Bool): Boolean =
         |  (x, y) match {
         |    case (True(), True()) => true
         |    case (True(), False()) => false
         |    case (x1, x2) => false
         |  }
         |""".stripMargin
    val fun = FunctionalXSouffleExecutor.loadFunction(code, souffleCode)
    println(fun.compiled.ir)
    // assert(fun.execute("main", Seq[meta.Term](q"True()", q"True()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(true))
    // assert(fun.execute("main", Seq[meta.Term](q"True()", q"False()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
    assert(fun.execute("main", Seq[meta.Term](q"False()", q"False()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
  }

  test("Tuple containing constructors having arguments") {
    val code =
      s"""module DataTuple
         |data Exp = Num(Int) | Var(String) | Add(Exp, Exp)
         |
         |@main def main(x: Exp, y: Exp): Boolean =
         |  (x, y) match {
         |    case (Num(i1), Num(i2)) => i1 == i2
         |    case (Var(s1), Var(s2)) => s1 == s2
         |    case (Add(l1, r1), Add(l2, r2)) => main(l1, l2) && main(r1, r2)
         |    case (x1, x2) => false
         |  }
         |""".stripMargin
    val fun = FunctionalXSouffleExecutor.loadFunction(code, souffleCode)
    println(fun.compiled.ir)
    // assert(fun.execute("main", Seq[meta.Term](q"Num(1)", q"Num(2)"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
    // assert(fun.execute("main", Seq[meta.Term](q"""Add(Num(1), Var("s"))""", q"""Add(Num(2), Var("s"))"""), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
    // assert(fun.execute("main", Seq[meta.Term](q"True()", q"False()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
    // assert(fun.execute("main", Seq[meta.Term](q"False()", q"False()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
    assert(fun.execute("main", Seq[meta.Term](q"""Add(Num(1), Num(1))""", q"""Add(Num(1), Var("s"))"""), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
    // assert(fun.execute("main", Seq[meta.Term](q"True()", q"False()"), Map[String, Seq[Seq[String]]](), false) == fun.resultVal(false))
  }
}
