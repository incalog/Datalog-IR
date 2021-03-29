package inca.frontend.lowering

import inca.backend.ir.GP
import inca.compiler.{Compiler, Options}
import inca.examples.ADT.Nat_lmi
import inca.examples.{AST, Code, ControlDataFlow, HigherOrder}
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class GenerateDatalogTest extends AnyFunSuite {

  def gpmodule(content: GP.Pattern*): GP.Module =
    GP.Module("Main", Seq(), Seq(), content, Seq())

  val baseExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7 + (12 * 3)"))),
    GP.Eq(GP.Var("out$0"), GP.Var("lit$0"))
  )))))

  val baseExampleGP2: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 12"))),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("lit$1") -> GP.TScalaInt, GP.Var("lit$2") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval$1"), GP.Evaluation(Seq(GP.Var("lit$0") -> GP.TScalaInt, GP.Var("eval$0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$1"))
  )))))


  val varExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit$0")),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit$1")),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 12"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("lit$2") -> GP.TScalaInt, GP.Var("y") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval$1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("eval$0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$1"))
  )))))

  val ifExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit$0")),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$1") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$0"), GP.True),
    GP.Eq(GP.Var("out$0"), GP.Var("x"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit$0")),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$1") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$0"), GP.False),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval$1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$2") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$1"))
  )))))


  val ifExample2GP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit$0")),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit$1")),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$2") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$0"), GP.True),
    GP.Computed(GP.Var("lit$4"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$2"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit$4") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$2"), GP.True),
    GP.Computed(GP.Var("eval$4"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("y") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$4"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit$0")),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit$1")),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$2") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$0"), GP.True),
    GP.Computed(GP.Var("lit$4"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$2"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit$4") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$2"), GP.False),
    GP.Computed(GP.Var("lit$5"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval$3"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit$5") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval$4"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("eval$3") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$4"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit$0")),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit$1")),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$2") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$0"), GP.False),
    GP.Computed(GP.Var("lit$3"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval$1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$3") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("lit$4"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$2"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit$4") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$2"), GP.True),
    GP.Computed(GP.Var("eval$4"), GP.Evaluation(Seq(GP.Var("eval$1") -> GP.TScalaInt, GP.Var("y") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$4"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit$0")),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit$1")),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$2") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$0"), GP.False),
    GP.Computed(GP.Var("lit$3"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval$1"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit$3") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("lit$4"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval$2"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit$4") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval$2"), GP.False),
    GP.Computed(GP.Var("lit$5"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval$3"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit$5") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval$4"), GP.Evaluation(Seq(GP.Var("eval$1") -> GP.TScalaInt, GP.Var("eval$3") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$4"))
  )))))



  val incFunGP = GP.Pattern(None, "inc", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit$0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$0"))
  ))))
  val incMainGP = GP.Pattern(None, "main", Seq(GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc", Seq(GP.Var("lit$0"), GP.Var("call$0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out$0"), GP.Var("call$0"))
  ))))
  val incModuleGP = gpmodule(incFunGP, incMainGP)




  val factFunGP = GP.Pattern(None, "fact", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit$0") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval$0"), GP.True),
    GP.Computed(GP.Var("lit$1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Eq(GP.Var("out$0"), GP.Var("lit$1"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit$0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval$0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit$0") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval$0"), GP.False),
    GP.Computed(GP.Var("lit$2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval$1"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit$2") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Call("fact", Seq(GP.Var("eval$1"), GP.Var("call$0")), transitive = false, neg = false),
    GP.Computed(GP.Var("eval$2"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("call$0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out$0"), GP.Var("eval$2"))
  ))))
  val factMainGP = GP.Pattern(None, "main", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out$0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Call("fact", Seq(GP.Var("n"), GP.Var("call$0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out$0"), GP.Var("call$0"))
  ))))
  val factModuleGP = gpmodule(factFunGP, factMainGP)



  val options = Options(Nat_lmi)

  test("base example") {
    val result = Compiler.compileFun(AST.baseExample, options).ir
    assert(result == baseExampleGP)
  }

  test("base example 2") {
    val result = Compiler.compileFun(AST.baseExample2, options).ir
    assert(result == baseExampleGP2)
  }

  test("var example") {
    val result = Compiler.compileFun(AST.varExample, options).ir
    assert(result == varExampleGP)
  }

  test("if example") {
    val result = Compiler.compileFun(AST.ifExample, options).ir
    assert(result == ifExampleGP)
  }

  test("if example 2") {
    val result = Compiler.compileFun(AST.ifExample2, options).ir
    assert(result == ifExample2GP)
  }

  test("inc example") {
    val result = Compiler.compileFun(AST.incModule, options).ir
    assert(result == incModuleGP)
  }

  test("fact example") {
    val result = Compiler.compileFun(AST.factModule, options).ir
    assert(result == factModuleGP)
  }

  test("plus example") {
    val result = Compiler.compileFun(AST.plusModule, options).ir
    println(result)
  }

  test("plus real example") {
    val result = Compiler.compileFun(AST.plusRealModule, options).ir
    println(result)
  }

  test("set constants") {
    val result = Compiler.compileFun(Code.setConstModule, options).ir
    println(result)
  }

  test("set operations") {
    val result = Compiler.compileFun(Code.setOperationsModule, options).ir
    println(result)
  }

  test("applyFun") {
    val result = Compiler.compileFun(HigherOrder.applyFun, options).ir
    println(result)
  }

  test("lambda") {
    val result = Compiler.compileFun(HigherOrder.lambda, options).ir
    println(result)
  }

  test("lambdaHigherOrder") {
    val result = Compiler.compileFun(HigherOrder.lambdaHigherOrder, options).ir
    println(result)
  }

  test("composeFun") {
    val result = Compiler.compileFun(HigherOrder.composeFun, options).ir
    println(result)
  }

  test("composeLambdas") {
    val result = Compiler.compileFun(HigherOrder.composeLambdas, options).ir
    println(result)
  }

  test("cflow") {
    val result = Compiler.compileFun(ControlDataFlow.cflowModule, options).ir
    println(result)
  }
}
