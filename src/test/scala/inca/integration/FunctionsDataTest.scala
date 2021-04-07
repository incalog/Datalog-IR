package inca.integration

import inca.Executor._
import inca.examples.{Code, LambdaCalculus}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
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
  test("section 5 Example") {
    val code = s"""module Test
                  |data Node = BusStation(`String`) | TrainStation(`String`) | NoStation()
                  |
                  |data Edge = Connection(Node, Node, `Int`)
                  |
                  |def eqNode(n1: Node, n2: Node): `Boolean` = n1 match {
                  |  case BusStation(name1) => n2 match {
                  |    case BusStation(name2) => name1 == name2
                  |    case TrainStation(name2) => false
                  |    case NoStation() => false
                  |  }
                  |  case TrainStation(name1) => n2 match {
                  |    case BusStation(name2) => false
                  |    case TrainStation(name2) => name1 == name2
                  |    case NoStation() => false
                  |  }
                  |  case NoStation() => n2 match {
                  |    case BusStation(name2) => false
                  |    case TrainStation(name2) => false
                  |    case NoStation() => true
                  |  }
                  |}
                  |
                  |def isBusStation(n: Node): `Boolean` = n match {
                  |  case BusStation(name) => true
                  |  case TrainStation(name) => false
                  |  case NoStation() => false
                  |}
                  |
                  |def connects(e: Edge, from: Node, to: Node): `Boolean` = e match {
                  |  case Connection(from1, to1, d) => eqNode(from, from1) && eqNode(to, to1)
                  |}
                  |
                  |def distance(e: Edge): `Int` = e match {
                  |  case Connection(f, t, d) => d
                  |}
                  |
                  |def connectedBusStations(from: Node): Set[(Node, `Int`)] =
                  | {(to, distance(e)) | to in Node, isBusStation(to), e in Edge, connects(e, from, to)}
                  |
                  |def connections(from: Node): Set[Edge] =
                  | {e | to in Node, isBusStation(to), e in Edge, connects(e, from, to)}
                  |
                  |// def connectedBusStations(from: Node): Set[(Node, `Int`)] =
                  |//  {(to, d) | to in Node, isBusStation(to), (from, to, d) in Edges()}
                  |
                  |// @main def main(): Set[(Node, `Int`)] = connectedBusStations(TrainStation("A"))
                  |@main def main(): Edge = nearestStation(TrainStation("A"))
                  |
                  |def minDistance(e1: Edge, e2: Edge): Edge = e1 match {
                  |  case Connection(from1, to1, d1) => e2 match {
                  |    case Connection(from2, to2, d2) => if (d1 < d2) e1 else e2
                  |  }
                  |}
                  |
                  |
                  |def nearestStation(from: Node): Edge = fold(Connection(NoStation(), NoStation(), `Int.MaxValue`), minDistance, connections(from))
                  |
                  |
                  |""".stripMargin
    val fun = loadFunction(code)
    fun.execute("main_f", Seq(q"""Connection(TrainStation("A"), BusStation("B"), 12)""", q"""Connection(TrainStation("A"), BusStation("C"), 5)"""))
    fun.printAllMatches()
  }

  test("Simple Fold Int Example") {
    val fun = loadFunction(Code.simpleFoldIntModule)
    val tuple = Tuples.flatTupleOf(fun.vals(q"1", q"10"):_*)
    assert(fun.executeTuple("sum_bbf", tuple) == fun.result(q"55"))
    assert(fun.output("AggregateCollection$0_bbf", tuple).res.size == 10)
    fun.printAllMatches()
  }

  test("Simple Fold Example") {
    val fun = loadFunction(Code.simpleFoldModule)
    val tuple = Tuples.flatTupleOf(fun.vals(q"1", q"10"):_*)
    assert(fun.executeTuple("sum_bbf", tuple) == fun.result(q"V(55)"))
    assert(fun.output("AggregateCollection$0_bbf", tuple).res.size == 10)
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
