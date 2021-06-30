package inca.frontend.functional.lowering

import inca.backend.ir.IR
import inca.compiler.Compiler
import inca.examples.functional.{AST, Code, ControlDataFlow, HigherOrder}
import inca.frontend.functional.compiler.FunctionalOptions
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class GenerateIRTest extends AnyFunSuite {

  def gpmodule(content: IR.Pattern*): IR.Module =
    IR.Module("Main", Seq(), content, Seq())

  val baseExampleGP: IR.Module = gpmodule(IR.Pattern(None, "main", Seq(IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7 + (12 * 3)"))),
    IR.Eq(IR.Var("out$0"), IR.Var("lit$0"))
  )))))

  val baseExampleGP2: IR.Module = gpmodule(IR.Pattern(None, "main", Seq(IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 12"))),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 3"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("lit$1") -> IR.TScalaInt, IR.Var("lit$2") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Computed(IR.Var("eval$1"), IR.Evaluation(Seq(IR.Var("lit$0") -> IR.TScalaInt, IR.Var("eval$0") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$1"))
  )))))


  val varExampleGP: IR.Module = gpmodule(IR.Pattern(None, "main", Seq(IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Eq(IR.Var("x"), IR.Var("lit$0")),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 3"))),
    IR.Eq(IR.Var("y"), IR.Var("lit$1")),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 12"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("lit$2") -> IR.TScalaInt, IR.Var("y") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Computed(IR.Var("eval$1"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("eval$0") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$1"))
  )))))

  val ifExampleGP: IR.Module = gpmodule(IR.Pattern(None, "main", Seq(IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Eq(IR.Var("x"), IR.Var("lit$0")),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$1") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$0"), IR.True),
    IR.Eq(IR.Var("out$0"), IR.Var("x"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Eq(IR.Var("x"), IR.Var("lit$0")),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$1") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$0"), IR.False),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -1"))),
    IR.Computed(IR.Var("eval$1"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$2") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$1"))
  )))))


  val ifExample2GP: IR.Module = gpmodule(IR.Pattern(None, "main", Seq(IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Eq(IR.Var("x"), IR.Var("lit$0")),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -3"))),
    IR.Eq(IR.Var("y"), IR.Var("lit$1")),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$2") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$0"), IR.True),
    IR.Computed(IR.Var("lit$4"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$2"), IR.Evaluation(Seq(IR.Var("y") -> IR.TScalaInt, IR.Var("lit$4") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$2"), IR.True),
    IR.Computed(IR.Var("eval$4"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("y") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$4"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Eq(IR.Var("x"), IR.Var("lit$0")),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -3"))),
    IR.Eq(IR.Var("y"), IR.Var("lit$1")),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$2") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$0"), IR.True),
    IR.Computed(IR.Var("lit$4"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$2"), IR.Evaluation(Seq(IR.Var("y") -> IR.TScalaInt, IR.Var("lit$4") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$2"), IR.False),
    IR.Computed(IR.Var("lit$5"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -1"))),
    IR.Computed(IR.Var("eval$3"), IR.Evaluation(Seq(IR.Var("y") -> IR.TScalaInt, IR.Var("lit$5") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Computed(IR.Var("eval$4"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("eval$3") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$4"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Eq(IR.Var("x"), IR.Var("lit$0")),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -3"))),
    IR.Eq(IR.Var("y"), IR.Var("lit$1")),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$2") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$0"), IR.False),
    IR.Computed(IR.Var("lit$3"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -1"))),
    IR.Computed(IR.Var("eval$1"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$3") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Computed(IR.Var("lit$4"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$2"), IR.Evaluation(Seq(IR.Var("y") -> IR.TScalaInt, IR.Var("lit$4") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$2"), IR.True),
    IR.Computed(IR.Var("eval$4"), IR.Evaluation(Seq(IR.Var("eval$1") -> IR.TScalaInt, IR.Var("y") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$4"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 7"))),
    IR.Eq(IR.Var("x"), IR.Var("lit$0")),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -3"))),
    IR.Eq(IR.Var("y"), IR.Var("lit$1")),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$2") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$0"), IR.False),
    IR.Computed(IR.Var("lit$3"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -1"))),
    IR.Computed(IR.Var("eval$1"), IR.Evaluation(Seq(IR.Var("x") -> IR.TScalaInt, IR.Var("lit$3") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Computed(IR.Var("lit$4"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Computed(IR.Var("eval$2"), IR.Evaluation(Seq(IR.Var("y") -> IR.TScalaInt, IR.Var("lit$4") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    IR.Eq(IR.Var("eval$2"), IR.False),
    IR.Computed(IR.Var("lit$5"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => -1"))),
    IR.Computed(IR.Var("eval$3"), IR.Evaluation(Seq(IR.Var("y") -> IR.TScalaInt, IR.Var("lit$5") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Computed(IR.Var("eval$4"), IR.Evaluation(Seq(IR.Var("eval$1") -> IR.TScalaInt, IR.Var("eval$3") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$4"))
  )))))



  val incFunGP = IR.Pattern(None, "inc", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit$0") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$0"))
  ))))
  val incMainGP = IR.Pattern(None, "main", Seq(IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Call("inc", Seq(IR.Var("lit$0"), IR.Var("call$0")), transitive = false, neg = false),
    IR.Eq(IR.Var("out$0"), IR.Var("call$0"))
  ))))
  val incModuleGP = gpmodule(incFunGP, incMainGP)




  val factFunGP = IR.Pattern(None, "fact", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit$0") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval$0"), IR.True),
    IR.Computed(IR.Var("lit$1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Eq(IR.Var("out$0"), IR.Var("lit$1"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit$0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval$0"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit$0") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval$0"), IR.False),
    IR.Computed(IR.Var("lit$2"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval$1"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit$2") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    IR.Call("fact", Seq(IR.Var("eval$1"), IR.Var("call$0")), transitive = false, neg = false),
    IR.Computed(IR.Var("eval$2"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("call$0") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Eq(IR.Var("out$0"), IR.Var("eval$2"))
  ))))
  val factMainGP = IR.Pattern(None, "main", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out$0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Call("fact", Seq(IR.Var("n"), IR.Var("call$0")), transitive = false, neg = false),
    IR.Eq(IR.Var("out$0"), IR.Var("call$0"))
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
    val result = Compiler.compileFunctional(AST.ifExample2, options).ir
    assert(result == ifExample2GP)
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
    val result = Compiler.compileFunctional(AST.plusModule, options).ir
    println(result)
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
