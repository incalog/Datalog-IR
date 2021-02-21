package inca.frontend.lowering

import inca.backend.ir.GP
import inca.compiler.{Compiler, Options}
import inca.examples.ADT.NAT_lmi
import inca.examples.AST
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class GenerateDatalogTest extends AnyFunSuite {

  def gpmodule(content: GP.Pattern*): GP.Module =
    GP.Module("Main", Seq(), content, Seq())

  val baseExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7 + (12 * 3)"))),
    GP.Eq(GP.Var("out"), GP.Var("lit"))
  )))))

  val baseExampleGP2: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 12"))),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("lit_0") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("lit") -> GP.TScalaInt, GP.Var("eval") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_0"))
  )))))


  val varExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit")),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit_0")),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 12"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("lit_1") -> GP.TScalaInt, GP.Var("y") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("eval") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_0"))
  )))))

  val ifExampleGP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit")),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_0") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Eq(GP.Var("out"), GP.Var("x"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit")),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_0") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_0"))
  )))))


  val ifExample2GP: GP.Module = gpmodule(GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit")),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit_0")),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Computed(GP.Var("lit_3"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit_3") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval_1"), GP.True),
    GP.Computed(GP.Var("eval_3"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("y") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_3"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit")),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit_0")),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Computed(GP.Var("lit_3"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit_3") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval_1"), GP.False),
    GP.Computed(GP.Var("lit_4"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval_2"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit_4") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval_3"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("eval_2") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_3"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit")),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit_0")),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_2") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("lit_3"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit_3") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval_1"), GP.True),
    GP.Computed(GP.Var("eval_3"), GP.Evaluation(Seq(GP.Var("eval_0") -> GP.TScalaInt, GP.Var("y") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_3"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 7"))),
    GP.Eq(GP.Var("x"), GP.Var("lit")),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -3"))),
    GP.Eq(GP.Var("y"), GP.Var("lit_0")),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_2"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("x") -> GP.TScalaInt, GP.Var("lit_2") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("lit_3"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit_3") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left > right"))),
    GP.Eq(GP.Var("eval_1"), GP.False),
    GP.Computed(GP.Var("lit_4"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => -1"))),
    GP.Computed(GP.Var("eval_2"), GP.Evaluation(Seq(GP.Var("y") -> GP.TScalaInt, GP.Var("lit_4") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Computed(GP.Var("eval_3"), GP.Evaluation(Seq(GP.Var("eval_0") -> GP.TScalaInt, GP.Var("eval_2") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_3"))
  )))))



  val incFunGP = GP.Pattern(None, "inc", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  ))))
  val incMainGP = GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val incModuleGP = gpmodule(incFunGP, incMainGP)




  val factFunGP = GP.Pattern(None, "fact", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Eq(GP.Var("out"), GP.Var("lit_0"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Call("fact", Seq(GP.Var("eval_0"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("out_0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  ))))
  val factMainGP = GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Call("fact", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val factModuleGP = gpmodule(factFunGP, factMainGP)



  val options = Options(NAT_lmi)

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
}
