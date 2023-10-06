package inca.backend.transform.monotype

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog.{AddMono, Body, Call, Computed, Eq, Evaluation, ExtensionalCall, MkMono, Module, Name, Param, Pattern, ResultMono, StringConstant, TScala, TScalaInt, TScalaString, Var}
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.frontend.ir.{EDBChange, Relation1, Relation2, Relation3, Datalog => DatalogAPI}
import inca.runtime.context.DataModel
import inca.util.Scala
import scala.meta._
import org.scalatest.funsuite.AnyFunSuiteLike


/**
 * Test whether the MonoTranslation can translate program
 * consisting of two mono-types correctly:
 *
 * ----------------------------------------------------------------------
 * size(t, m) :- leaf(t), m <- (t, 1).
 *            :- btree(t, l, r), size(<l>, <m>), size(<r>, <m>), m <- (t, 1).
 * main(<t>, >b1<, >b2<) :- MkMono(m1, CountMono), MkMono(m2, CountMono),
 *                          size(<t>, <m1>),t = "A", m1 -> b1, m2 -> b2.
 * ----------------------------------------------------------------------
 *
 * The assumed output for the main relation is ("A", 5, 0).
 */

class TreeSize2Test extends AnyFunSuiteLike {
  test("Two mono type instances") {
    val treeSize1: Module = {
      lazy val pat1Body1: Body = Body(Seq(
        ExtensionalCall("leaf", Seq(Var("t"))),
        Computed(Var("a"), Evaluation(
          Seq(Var("t") -> TScalaString),
          TScala(Scala(t"(String, Int)")),
          Scala(q"(t : String) => (t, 1)")
        )),
        AddMono(Var("m"), Var("a"))
      ))

      lazy val pat1Body2: Body = Body(Seq(
        ExtensionalCall("btree", Seq(Var("t"), Var("l"), Var("r"))),
        Call("size", Seq(Var("l"), Var("m"))).addHint(MagicSetHints.FixedAdornment(Seq(true, true))),
        Call("size", Seq(Var("r"), Var("m"))).addHint(MagicSetHints.FixedAdornment(Seq(true, true))),
        Computed(Var("a"), Evaluation(
          Seq(Var("t") -> TScalaString),
          TScala(Scala(t"(String, Int)")),
          Scala(q"(t : String) => (t, 1)")
        )),
        AddMono(Var("m"), Var("a"))
      ))

      // main(t, v) :- MkMono(m, CountMono), size(t, m),t = "A", m -> v
      lazy val pat2Body: Body = Body(Seq(
        MkMono(Var("m"), "inca.backend.transform.monotype.CountMono"),
        Eq(Var("t"), StringConstant("A")),
        Call("size", Seq(Var("t"), Var("m"))).addHint(MagicSetHints.FixedAdornment(Seq(true, true))),
        ResultMono(Var("m"), Var("v")),
        MkMono(Var("m2"), "inca.backend.transform.monotype.CountMono"),
        ResultMono(Var("m2"), Var("v1")),
      ))

      lazy val pat1: Pattern = Pattern(
        None, "size",
        Seq(Param("t", TScalaString),
          Param("m", TScala(Scala(t"inca.backend.transform.monotype.CountMono")))),
        Seq(pat1Body1, pat1Body2)
      )

      lazy val pat2: Pattern = Pattern(
        None, "main",
        Seq(
          Param("t", TScalaString),
          Param("v", TScalaInt),
          Param("v1", TScalaInt)
        ),
        Seq(pat2Body)
      ).addHint(MagicSetHints.Main(Seq(true, false, false)))


      Module(
        "treeSize2", Seq(), Seq(pat1, pat2), Seq()
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

      override def name: Name = "treeSize2"

      override def sourceLocation: SourceLocation = ???

      override def ir: Module = treeSize1

      override def dataModel: DataModel = new DataModel()
    }

    val prog: DatalogAPI = new DatalogAPI(compiledModule)
    val edb: EDBChange = EDBChange.insertions(
      Seq(
        Relation3("btree", Seq("t", "l", "r"), Seq(Seq("A", "B", "C"), Seq("C", "D", "E"))),
        Relation1("leaf", Seq("t"), Seq(Seq("B"), Seq("D"), Seq("E"))),
        Relation1("ext_input$main$bff", Seq("t"), Seq(Seq("A"))),
      )
    )
    prog.update(edb)
    // Bug: prog.read(Relation2("main", Seq("m", "b1"), Seq())) also produces the same result
    val res = prog.read(Relation3("main", Seq("m", "b1", "b2"), Seq()))
    assert(res.size == 1)
    assert(res.toSet.toList.head == ("A", 5, 5))
  }
}
