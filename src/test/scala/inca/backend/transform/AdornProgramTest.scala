package inca.backend.transform

import inca.backend.transform.magic.{Adornment, AdornmentTag, Bound, Free}
import org.scalatest.funsuite.AnyFunSuite
import inca.backend.ir.GP
import inca.frontend.examples.AST
import inca.frontend.lowering.GenerateDatalog
import inca.util.Meta.Scala

import scala.meta.quasiquotes._


class AdornProgramTest extends AnyFunSuite {

  def gpmodule(content: GP.Pattern*): GP.Module =
    GP.Module("Main", Seq(), content, Seq())

  val incFunGP = GP.Pattern(None, "inc_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  ))))
  val incMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val incModuleGP = gpmodule(incFunGP, incMainGP)



  val factFunGP = GP.Pattern(None, "fact_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
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
    GP.Call("fact_bf", Seq(GP.Var("eval_0"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("out_0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  ))))
  val factMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Call("fact_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val factModuleGP = gpmodule(factFunGP, factMainGP)

  test("Adornment of flat function") {
    val trans = new Adornment {
      override def query: GP.Call = GP.Call("main", Seq(GP.Var("out")), transitive = false, neg = false)
      override def adornmentTags: Seq[AdornmentTag] = Seq(Free)
    }

    val gpModule = GenerateDatalog.transformModule(AST.incModule)
    val adorned = trans.transformer.transformModule(gpModule)
    assert(moduleEqual(adorned, incModuleGP))
  }

  def moduleEqual(m1: GP.Module, m2: GP.Module): Boolean =
    m1.pats.size == m2.pats.size && m1.pats.forall(m2.pats.contains)

  test("Adornment of recursive function") {
    val trans = new Adornment {
      override def query: GP.Call = GP.Call("main", Seq(GP.Var("out")), transitive = false, neg = false)
      override def adornmentTags: Seq[AdornmentTag] = Seq(Free)
    }

    val gpModule = GenerateDatalog.transformModule(AST.factModule)
    val adorned = trans.transformer.transformModule(gpModule)
    assert(moduleEqual(adorned, factModuleGP))
  }
}
