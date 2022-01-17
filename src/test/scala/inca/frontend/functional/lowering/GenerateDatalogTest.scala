package inca.frontend.functional.lowering

import inca.backend.ir.Datalog
import inca.compiler.Compiler
import inca.compiler.options.FunctionalOptions
import inca.examples.functional.{AST, Code, ControlDataFlow, HigherOrder}
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class GenerateDatalogTest extends AnyFunSuite {

  def gpmodule(content: Datalog.Pattern*): Datalog.Module =
    Datalog.Module("Main", Seq(), content, Seq())

  val baseExampleGP: Datalog.Module = gpmodule(Datalog.Pattern(None, "main", Seq(Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7 + (12 * 3)"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("lit$0"))
  )))))

  val baseExampleGP2: Datalog.Module = gpmodule(Datalog.Pattern(None, "main", Seq(Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 12"))),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("lit$1") -> Datalog.TScalaInt, Datalog.Var("lit$2") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Computed(Datalog.Var("eval$1"), Datalog.Evaluation(Seq(Datalog.Var("lit$0") -> Datalog.TScalaInt, Datalog.Var("eval$0") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$1"))
  )))))


  val varExampleGP: Datalog.Module = gpmodule(Datalog.Pattern(None, "main", Seq(Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Eq(Datalog.Var("x"), Datalog.Var("lit$0")),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))),
    Datalog.Eq(Datalog.Var("y"), Datalog.Var("lit$1")),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 12"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("lit$2") -> Datalog.TScalaInt, Datalog.Var("y") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Computed(Datalog.Var("eval$1"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("eval$0") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$1"))
  )))))

  val ifExampleGP: Datalog.Module = gpmodule(Datalog.Pattern(None, "main", Seq(Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Eq(Datalog.Var("x"), Datalog.Var("lit$0")),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$1") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.True),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("x"))
  )), Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Eq(Datalog.Var("x"), Datalog.Var("lit$0")),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$1") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.False),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -1"))),
    Datalog.Computed(Datalog.Var("eval$1"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$2") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$1"))
  )))))


  val ifExample2GP: Datalog.Module = gpmodule(Datalog.Pattern(None, "main", Seq(Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Eq(Datalog.Var("x"), Datalog.Var("lit$0")),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -3"))),
    Datalog.Eq(Datalog.Var("y"), Datalog.Var("lit$1")),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$2") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.True),
    Datalog.Computed(Datalog.Var("lit$4"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$2"), Datalog.Evaluation(Seq(Datalog.Var("y") -> Datalog.TScalaInt, Datalog.Var("lit$4") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$2"), Datalog.True),
    Datalog.Computed(Datalog.Var("eval$4"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("y") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$4"))
  )), Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Eq(Datalog.Var("x"), Datalog.Var("lit$0")),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -3"))),
    Datalog.Eq(Datalog.Var("y"), Datalog.Var("lit$1")),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$2") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.True),
    Datalog.Computed(Datalog.Var("lit$4"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$2"), Datalog.Evaluation(Seq(Datalog.Var("y") -> Datalog.TScalaInt, Datalog.Var("lit$4") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$2"), Datalog.False),
    Datalog.Computed(Datalog.Var("lit$5"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -1"))),
    Datalog.Computed(Datalog.Var("eval$3"), Datalog.Evaluation(Seq(Datalog.Var("y") -> Datalog.TScalaInt, Datalog.Var("lit$5") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Computed(Datalog.Var("eval$4"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("eval$3") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$4"))
  )), Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Eq(Datalog.Var("x"), Datalog.Var("lit$0")),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -3"))),
    Datalog.Eq(Datalog.Var("y"), Datalog.Var("lit$1")),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$2") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.False),
    Datalog.Computed(Datalog.Var("lit$3"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -1"))),
    Datalog.Computed(Datalog.Var("eval$1"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$3") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Computed(Datalog.Var("lit$4"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$2"), Datalog.Evaluation(Seq(Datalog.Var("y") -> Datalog.TScalaInt, Datalog.Var("lit$4") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$2"), Datalog.True),
    Datalog.Computed(Datalog.Var("eval$4"), Datalog.Evaluation(Seq(Datalog.Var("eval$1") -> Datalog.TScalaInt, Datalog.Var("y") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$4"))
  )), Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))),
    Datalog.Eq(Datalog.Var("x"), Datalog.Var("lit$0")),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -3"))),
    Datalog.Eq(Datalog.Var("y"), Datalog.Var("lit$1")),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$2") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.False),
    Datalog.Computed(Datalog.Var("lit$3"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -1"))),
    Datalog.Computed(Datalog.Var("eval$1"), Datalog.Evaluation(Seq(Datalog.Var("x") -> Datalog.TScalaInt, Datalog.Var("lit$3") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Computed(Datalog.Var("lit$4"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Computed(Datalog.Var("eval$2"), Datalog.Evaluation(Seq(Datalog.Var("y") -> Datalog.TScalaInt, Datalog.Var("lit$4") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    Datalog.Eq(Datalog.Var("eval$2"), Datalog.False),
    Datalog.Computed(Datalog.Var("lit$5"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => -1"))),
    Datalog.Computed(Datalog.Var("eval$3"), Datalog.Evaluation(Seq(Datalog.Var("y") -> Datalog.TScalaInt, Datalog.Var("lit$5") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Computed(Datalog.Var("eval$4"), Datalog.Evaluation(Seq(Datalog.Var("eval$1") -> Datalog.TScalaInt, Datalog.Var("eval$3") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$4"))
  )))))



  val incFunGP = Datalog.Pattern(None, "inc", Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit$0") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$0"))
  ))))
  val incMainGP = Datalog.Pattern(None, "main", Seq(Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))),
    Datalog.Call("inc", Seq(Datalog.Var("lit$0"), Datalog.Var("call$0")), transitive = false, neg = false),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("call$0"))
  ))))
  val incModuleGP = gpmodule(incFunGP, incMainGP)




  val factFunGP = Datalog.Pattern(None, "fact", Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit$0") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.True),
    Datalog.Computed(Datalog.Var("lit$1"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("lit$1"))
  )), Datalog.Body(Seq(
    Datalog.Computed(Datalog.Var("lit$0"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
    Datalog.Computed(Datalog.Var("eval$0"), Datalog.Evaluation(Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit$0") -> Datalog.TScalaInt),
      Datalog.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    Datalog.Eq(Datalog.Var("eval$0"), Datalog.False),
    Datalog.Computed(Datalog.Var("lit$2"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
    Datalog.Computed(Datalog.Var("eval$1"), Datalog.Evaluation(Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit$2") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    Datalog.Call("fact", Seq(Datalog.Var("eval$1"), Datalog.Var("call$0")), transitive = false, neg = false),
    Datalog.Computed(Datalog.Var("eval$2"), Datalog.Evaluation(Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("call$0") -> Datalog.TScalaInt),
      Datalog.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("eval$2"))
  ))))
  val factMainGP = Datalog.Pattern(None, "main", Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out$0", Datalog.TScalaInt)), Seq(Datalog.Body(Seq(
    Datalog.Call("fact", Seq(Datalog.Var("n"), Datalog.Var("call$0")), transitive = false, neg = false),
    Datalog.Eq(Datalog.Var("out$0"), Datalog.Var("call$0"))
  ))))
  val factModuleGP = gpmodule(factFunGP, factMainGP)



  val options = FunctionalOptions()

  test("base example") {
    val result = Compiler.compileFunctional(AST.baseExample, options).ir
    assert(result == baseExampleGP)
  }

  test("base example 2") {
    val result = Compiler.compileFunctional(AST.baseExample2, options).ir
    assert(result == baseExampleGP2)
  }

  test("var example") {
    val result = Compiler.compileFunctional(AST.varExample, options).ir
    assert(result == varExampleGP)
  }

  test("if example") {
    val result = Compiler.compileFunctional(AST.ifExample, options).ir
    assert(result == ifExampleGP)
  }

  test("if example 2") {
    val compiled = Compiler.compileFunctional(AST.ifExample2, options)
    println(compiled.fun)
    println(compiled.ir)
    println(compiled.transformed)
    println(compiled.optimized)
    assert(compiled.ir == ifExample2GP)
  }

  test("inc example") {
    val result = Compiler.compileFunctional(AST.incModule, options).ir
    assert(result == incModuleGP)
  }

  test("fact example") {
    val result = Compiler.compileFunctional(AST.factModule, options).ir
    assert(result == factModuleGP)
  }

  test("plus example") {
    val compiled = Compiler.compileFunctional(AST.plusModule, options)
    println(compiled.fun)
    println(compiled.ir)
    println(compiled.transformed)
    println(compiled.optimized)
  }
//  test("running example for section 5") {
//    val result = Compiler.compileFunctional(
//      s"""module Test
//         |data Node = BusStation(String) | TrainStation(String)
//         |
//         |def Edges(): Set[(Node, Node, Int)] = {
//         |  (TrainStation("A"), BusStation("B"), 12),
//         |  (TrainStation("A"), BusStation("C"), 5),
//         |  (TrainStation("D"), BusStation("B"), 145),
//         |  (BusStation("B"), BusStation("C"), 1)
//         |}
//         |
//         |def isBusStation(n: Node): `Boolean` = n match {
//         |  case BusStation(name) => true
//         |  case TrainStation(name) => false
//         |}
//         |
//         |def connectedBusStations(from: Node): Set[Node] =
//         | {to | isBusStation(to), (from, to, d) in Edges()}
//         |
//         |@main def main(): Set[Node] = connectedBusStations(TrainStation("A"))
//         |
//         |
//         |""".stripMargin, options).ir
//    println(result)
//  }

  test("plus real example") {
    val result = Compiler.compileFunctional(AST.plusRealModule, options).ir
    println(result)
  }

  test("set constants") {
    val result = Compiler.compileFunctional(Code.setConstModule, options).ir
    println(result)
  }

  test("set operations") {
    val result = Compiler.compileFunctional(Code.setOperationsModule, options).ir
    println(result)
  }

  test("applyFun") {
    val result = Compiler.compileFunctional(HigherOrder.applyFun, options).ir
    println(result)
  }

  test("lambda") {
    val result = Compiler.compileFunctional(HigherOrder.lambda, options).ir
    println(result)
  }

  test("lambdaHigherOrder") {
    val result = Compiler.compileFunctional(HigherOrder.lambdaHigherOrder, options).ir
    println(result)
  }

  test("composeFun") {
    val result = Compiler.compileFunctional(HigherOrder.composeFun, options).ir
    println(result)
  }

  test("composeLambdas") {
    val result = Compiler.compileFunctional(HigherOrder.composeLambdas, options).ir
    println(result)
  }

  test("cflow") {
    val result = Compiler.compileFunctional(ControlDataFlow.cflowModule, options).ir
    println(result)
  }
}
