package inca.frontend.functional.integration

import inca.examples.functional.Code
import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.executor.FunctionalExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.ArraySeq
import scala.meta.XtensionQuasiquoteTerm

class FunctionsTest extends AnyFunSuite {
  test("Factorial Example") {
    val fun = FunctionalExecutor.loadFunction(Code.factModule)
//    println(fun.compiled.optimized)
    assert(fun.execute("main", Seq(q"5")) == fun.resultVal(120))
    assert(fun.execute("main", Seq(q"10")) == fun.resultVal(3628800))
//    fun.printAllMatches()
  }

  test("Fibonacci Example") {
    val fun = FunctionalExecutor.loadFunction(Code.fibModule)
    assert(fun.execute("main", Seq(q"10")) == fun.resultVal(55))
    assert(fun.execute("main", Seq(q"11")) == fun.resultVal(89))
    assert(fun.execute("main", Seq(q"20")) == fun.resultVal(6765))
//    println(fun.compiled.optimized)
//    fun.printAllMatches()
  }

  test("Simple Set Intersection") {
    val code =
      s"""module SetIntersection
         |@main def main(): Set[Int] = {1, 2, 3} & {1, 3}
         |""".stripMargin
    val fun = FunctionalExecutor.loadFunction(code)
    // TODO: fix
    assert(fun.execute("main", Seq()) == fun.results(Seq(Seq(1), Seq(3))))
  }

  test("Complex Set Intersection") {
    val code =
      s"""module SetIntersection
         |def A(): Set[Int] = {1, 2, 3, 4, 5}
         |def B(): Set[Int] = {3, 4, 5}
         |def C(): Set[Int] = {1, 2, 3, 4}
         |def D(): Set[Int] = {15}
         |@main def main(): Set[Int] = A() & B() & C()
         |@main def main2(): Set[Int] = {(x*x) | x in A()} & { y | y in C()}
         |@main def main3(): Set[Int] = {(x*x) | x in A()} & {(x+1) | x in D()}
         |@main def main4(): Set[Int] =
         |  let x = 5 in
         |    {(y*x) | y in A()} & {(y+x) | y in B()}
         |@main def main5(): Set[Int] =
         |  {x | x in A()} & {x | x in B()} & {x | x in C()}
         |""".stripMargin
    val fun = FunctionalExecutor.loadFunction(code, FunctionalOptions().withOptimizations(Seq()))
    println(fun.compiled.transformed)
    assert(fun.execute("main", Seq()) == fun.results(Seq(Seq(3), Seq(4))))
    assert(fun.execute("main2", Seq()) == fun.results(Seq(Seq(1), Seq(4))))
    assert(fun.execute("main3", Seq()) == fun.results(Seq(Seq(16))))
    assert(fun.execute("main4", Seq()) == fun.results(Seq(Seq(10))))
    assert(fun.execute("main5", Seq()) == fun.results(Seq(Seq(3), Seq(4))))
  }
}
