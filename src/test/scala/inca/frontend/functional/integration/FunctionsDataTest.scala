package inca.frontend.functional.integration

import inca.backend.analyze.DependencyGraph
import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix
import inca.examples.functional.{Code, LambdaCalculus}
import inca.frontend.functional.executor.FunctionalExecutor._
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionsDataTest extends AnyFunSuite {

  test("Plus Example") {
    val fun = loadFunction(Code.plusRealModule)
    assert(fun.execute("main", Seq(q"Succ(Succ(Zero()))", q"Succ(Zero())"))
      == fun.result(q"Succ(Succ(Succ(Zero())))"))
    assert(fun.execute("main", Seq(q"Succ(Succ(Succ(Succ(Zero()))))", q"Succ(Succ(Zero()))"))
      == fun.result(q"Succ(Succ(Succ(Succ(Succ(Succ(Zero()))))))"))
//    fun.printAllMatches()
  }

  test("graph example with functions as predicates") {
    val code = s"""module Test
                  |data Node = BusStation(String, Int) | TrainStation(String, Int) | NoStation()
                  |
                  |def stations(): Set[Node] = { TrainStation("A", 10), BusStation("B", 5), BusStation("C", 2) }
                  |
                  |def isBusStation(n: Node): `Boolean` = n match {
                  |  case BusStation(name, cap) => true
                  |  case TrainStation(name, cap) => false
                  |  case NoStation() => false
                  |}
                  |
                  |def capacity(n: Node): Int = n match {
                  |  case BusStation(name, cap) => cap
                  |  case TrainStation(name, cap) => cap
                  |  case NoStation() => -1
                  |}
                  |
                  |def maxCapacity(n1: Node, n2: Node): Node = if (capacity(n1) > capacity(n2)) n1 else n2
                  |
                  |@main def main(): Node = fold(NoStation(), maxCapacity, {n | n in stations(), isBusStation(n)})
                  |""".stripMargin
    val fun = loadFunction(code)
    assert(fun.execute("main", Seq()) == fun.result(q"""BusStation("B", 5)"""))
//    fun.printAllMatches()
  }

  test("Simple Fold Int Example") {
    val fun = loadFunction(Code.simpleFoldIntModule)
//    val tuple = Tuples.flatTupleOf(fun.vals(q"1", q"10"):_*)
    assert(fun.execute("sum", Seq(q"1", q"10")) == fun.result(q"55"))
//    assert(fun.output("AggregateCollection$0", tuple).res.size == 10)
//    fun.printAllMatches()
  }

  test("Simple Fold Example") {
    val fun = loadFunction(Code.simpleFoldModule)
//    val tuple = Tuples.flatTupleOf(fun.vals(q"1", q"10"):_*)
    fun.execute("sum", Seq(q"1", q"10"))
//    fun.printAllMatches()
//    assert(fun.output("sum", tuple) == fun.result(q"V(55)"))
//    assert(fun.output("AggregateCollection$0", tuple).res.size == 10)
  }

  test("Type Checker Example") {
    val fun = loadFunction(LambdaCalculus.typeOfModule)
    println(new DependencyGraph(fun.compiled.optimized).toGraphViz)
    assert(fun.execute("main", Seq(q"TNum(1)"))
      == fun.result(q"SomeType(TInt())"))
    assert(fun.execute("main", Seq(q"""TLam("x", TInt(), TVar("x"))"""))
      == fun.result(q"SomeType(TFun(TInt(), TInt()))"))
    assert(fun.execute("main", Seq(q"""TLam("x", TInt(), TVar("y"))"""))
      == fun.result(q"NoType()"))
    assert(fun.execute("main", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""))
      == fun.result(q"SomeType(TInt())"))
    assert(fun.execute("main", Seq(q"""TApp(TNum(12), TNum(11))"""))
      == fun.result(q"NoType()"))
//    fun.printAllMatches()
  }

  test("Type Checker Relation Example") {
    val fun = loadFunction(LambdaCalculus.typeOfRelModule)
    assert(fun.execute("main", Seq(q"TNum(1)"))
      == fun.result(q"TInt()"))
    assert(fun.execute("main", Seq(q"""TLam("x", TInt(), TVar("x"))"""))
      == fun.result(q"TFun(TInt(), TInt())"))
    assert(fun.execute("main", Seq(q"""TLam("x", TInt(), TVar("y"))"""))
      == fun.results(Seq()))
    assert(fun.execute("main", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""))
      == fun.result(q"TInt()"))
    assert(fun.execute("main", Seq(q"""TApp(TNum(12), TNum(11))"""))
      == fun.results(Seq()))
//    fun.printAllMatches()
  }

  test("Type Erasure Example") {
    val fun = loadFunction(LambdaCalculus.eraseModule)
    assert(fun.execute("main", Seq(q"TNum(1)"))
      == fun.result(q"Num(1)"))
    assert(fun.execute("main", Seq(q"""TLam("x", TInt(), TVar("x"))"""))
      == fun.result(q"""Lam("x", Var("x"))"""))
    assert(fun.execute("main", Seq(q"""TLam("x", TInt(), TVar("y"))"""))
      == fun.result(q"""Lam("x", Var("y"))"""))
    assert(fun.execute("main", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""))
      == fun.result(q"""App(Lam("x", Var("x")), Num(1337))"""))
    assert(fun.execute("main", Seq(q"""TApp(TNum(12), TNum(11))"""))
      == fun.result(q"App(Num(12), Num(11))"))
//    fun.printAllMatches()
  }

  test("Interpreter Example") {
    val fun = loadFunction(LambdaCalculus.interpModule)
    assert(fun.execute("main", Seq(q"Num(1)"))
      == fun.result(q"SomeVal(VNum(1))"))
    assert(fun.execute("main", Seq(q"""Lam("x", Var("x"))"""))
      == fun.result(q"""SomeVal(VClosure("x", Var("x"), EmptyEnv()))"""))
    assert(fun.execute("main", Seq(q"""Lam("x", Var("y"))"""))
      == fun.result(q"""SomeVal(VClosure("x", Var("y"), EmptyEnv()))"""))
    assert(fun.execute("main", Seq(q"""App(Lam("y", Lam("x", Var("y"))), Num(1))"""))
      == fun.result(q"""SomeVal(VClosure("x", Var("y"), BindEnv("y", VNum(1), EmptyEnv())))"""))
    assert(fun.execute("main", Seq(q"""App(Lam("x", Var("y")), Num(1))"""))
      == fun.result(q"""NoVal()"""))
    assert(fun.execute("main", Seq(q"""App(Lam("x", Var("x")), Num(1337))"""))
      == fun.result(q"""SomeVal(VNum(1337))"""))
    assert(fun.execute("main", Seq(q"""App(Num(12), Num(11))"""))
      == fun.result(q"NoVal()"))
//    fun.printAllMatches()
  }

  test("Checking+Erasure+Interpreting Example") {
    val fun = loadFunction(LambdaCalculus.completeLCModule)

    val rels = fun.compiled.optimized.pats
    println("relations: " + rels.size)
    println("input relations: " + rels.count(_.name.contains(demandPatternPrefix)))
    println("bodies: " + rels.flatMap(_.bodies).size)
    println("atoms: " + rels.flatMap(_.bodies.flatMap(_.atoms)).size)

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
    assert(fun.execute("main", Seq(three))
      == fun.result(q"""SomeVal(VClosure("f", Lam("x", App(Var("f"), App(Var("f"), App(Var("f"), Var("x"))))), EmptyEnv()))"""))

    // TODO how to assert result?
    fun.execute("main", Seq(q"TApp($succ, $three)"))
    fun.printMatches("main")
    fun.execute("main", Seq(q"TApp(TApp($plus, TApp($succ, $three)), $one)"))
    fun.printMatches("main")
  }
}
