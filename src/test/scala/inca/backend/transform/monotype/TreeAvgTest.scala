package inca.backend.transform.monotype

import scala.meta._
import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog.{AddMono, Body, Call, Computed, Eq, Evaluation, ExtensionalCall, MkMono, Module, Name, Param, Pattern, ResultMono, StringConstant, TScala, TScalaInt, TScalaString, Var}
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.frontend.ir.{EDBChange, Relation1, Relation2, Relation3, Datalog => DatalogAPI}
import inca.runtime.context.DataModel
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuiteLike

/**
 * Test case: Two AddMono atoms in the same rule body, they have different mono-types.
 *
 * avg(t, m1, m2) :- leaf(t), label(t, a), m1 <- (t, a), m2 <- (t, a)
 *                :- btree(t, l, r), label(t, a), m1 <- (t, a), m2 <- (t, a)
 * main(t, b1, b2) :- m1 = AddMono@(String, Int), m2 = ProdMono@(String, Int),
 *                    avg("A", m1, m2), m1 -> b1, m2 -> b2
 */

class TreeAvgTest extends AnyFunSuiteLike {
  test("Two mono type instances") {
    val treeSize1: Module = {
      lazy val pat1Body1: Body = Body(Seq(
        ExtensionalCall("leaf", Seq(Var("t"))),
        ExtensionalCall("label", Seq(Var("t"), Var("n"))),
        Computed(Var("a"), Evaluation(
          Seq(Var("t") -> TScalaString, Var("n") -> TScalaInt),
          TScala(Scala(t"(String, Int)")),
          Scala(q"(t : String, n : Int) => (t, n)")
        )),
        AddMono(Var("m1"), Var("a")),
        AddMono(Var("m2"), Var("a"))
      ))

      lazy val pat1Body2: Body = Body(Seq(
        ExtensionalCall("btree", Seq(Var("t"), Var("l"), Var("r"))),
        ExtensionalCall("label", Seq(Var("t"), Var("n"))),
        Call("ps", Seq(Var("l"), Var("m1"), Var("m2"))).addHint(MagicSetHints.FixedAdornment(Seq(true, true, true))),
        Call("ps", Seq(Var("r"), Var("m1"), Var("m2"))).addHint(MagicSetHints.FixedAdornment(Seq(true, true, true))),
        Computed(Var("a"), Evaluation(
          Seq(Var("t") -> TScalaString, Var("n") -> TScalaInt),
          TScala(Scala(t"(String, Int)")),
          Scala(q"(t : String, n : Int) => (t, n)")
        )),
        AddMono(Var("m1"), Var("a")),
        AddMono(Var("m2"), Var("a"))
      ))

      lazy val pat2Body: Body = Body(Seq(
        MkMono(Var("m1"), TScala(Scala(t"inca.backend.transform.monotype.AddMono")), Seq(TScalaString, TScalaInt)),
        MkMono(Var("m2"), TScala(Scala(t"inca.backend.transform.monotype.ProdMono")), Seq(TScalaString, TScalaInt)),
        Eq(Var("t"), StringConstant("A")),
        Call("ps", Seq(Var("t"), Var("m1"), Var("m2"))).addHint(MagicSetHints.FixedAdornment(Seq(true, true, true))),
        ResultMono(Var("m1"), Var("v1")),
        ResultMono(Var("m2"), Var("v2"))
      ))

      lazy val pat1: Pattern = Pattern(
        None, "ps",
        Seq(
          Param("t", TScalaString),
          Param("m1", TScala(Scala(t"inca.backend.transform.monotype.AddMono"))),
          Param("m2", TScala(Scala(t"inca.backend.transform.monotype.ProdMono")))
        ),
        Seq(pat1Body1, pat1Body2)
      )

      lazy val pat2: Pattern = Pattern(
        None, "main",
        Seq(
          Param("t", TScalaString),
          Param("v1", TScalaInt),
          Param("v2", TScalaInt)
        ),
        Seq(pat2Body)
      ).addHint(MagicSetHints.Main(Seq(true, false, false)))


      Module(
        "treeAddProd", Seq(), Seq(pat1, pat2), Seq()
      )
    }

    val compiledModule = new CompiledModule {
      override val options: Options = new Options {
        override def optimizations: Seq[Optimization] = Seq()

        override def transformations: Seq[Transformation] = Seq(
          DeriveDemandPatterns,
          DemandTransformation,
          MonoTransformation
        )

        override def stopOnError: Boolean = true

        override def stopOnWarning: Boolean = true

        override def withOptimizations(opts: Seq[Optimization]): Options = ???

        override def withTransformations(trans: Seq[Transformation]): Options = ???
      }

      override def name: Name = "treeAddProd"

      override def sourceLocation: SourceLocation = ???

      override def ir: Module = treeSize1

      override def dataModel: DataModel = new DataModel()
    }

    val prog: DatalogAPI = new DatalogAPI(compiledModule)
    val edb: EDBChange = EDBChange.insertions(
      Seq(
        Relation3("btree", Seq("t", "l", "r"), Seq(Seq("A", "B", "C"), Seq("C", "D", "E"))),
        Relation1("leaf", Seq("t"), Seq(Seq("B"), Seq("D"), Seq("E"))),
        Relation2("label", Seq("t", "l"), Seq(Seq("A", 1), Seq("B", 2), Seq("C", 3), Seq("D", 4), Seq("E", 5))),
        Relation1("ext_input$main$bff", Seq("t"), Seq(Seq("A"))),
      )
    )
    prog.update(edb)
    println(prog.readAll)
    val res = prog.read(Relation3("main", Seq("m", "b1", "b2"), Seq()))
    assert(res.toSet.toList.head == ("A", 15, 120))
  }
}
