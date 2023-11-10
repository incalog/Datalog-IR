package inca.backend.transform.monotype

import inca.analyzedData.Nat.Nat

import scala.meta._
import inca.frontend.ir.{EDBChange, Relation1, Relation2, Relation3, Datalog => DatalogAPI}
import inca.backend.ir.Datalog.{AddMono, Body, Call, Computed, CustomAggregation, Eq, Evaluation, ExtensionalCall, IntConstant, MkMono, Module, Name, Param, Pattern, ResultMono, StringConstant, TScala, TScalaInt, TScalaString, Var}
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.util.Scala
import inca.compiler.{CompiledModule, Options, SourceLocation}
import org.scalatest.funsuite.AnyFunSuiteLike
import inca.backend.hints.MagicSetHints
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.frontend.constraint.core.Aggregate
import inca.runtime.aggregate.JoinAggregation
import inca.runtime.context.DataModel

/**
 * Test whether the MonoTranslation can translate program
 * consisting of one mono-type variable correctly:
 *
 * ----------------------------------------------------------------------
 * size(t, m) :- leaf(t), m <- (t, 1).
 *            :- btree(t, l, r), size(<l>, <m>),
 *               size(<r>, <m>), m <- (t, 1).
 * main(<t>, >b<) :- MkMono(m, CountMono), size(<t>, <m>),t = "A", m -> b.
 * ----------------------------------------------------------------------
 *
 * The assumed output for the main relation is ("A", 5).
 *
 * Agg(a, t) :- Coll(a, b, t)
 * main(b) :- c = Aggregate(Agg(a, #))
 */
class sumAgg extends JoinAggregation[Int] {
  override val name: String = "sum"

  override def init: Int = 0

  override def join(v1: Int, v2: Int): Int = {
    val s = v1 + v2
    //      println(s"Adding ${v1.toInt} + ${v2.toInt} = ${s.toInt}")
    s
  }

  override val isAssociative: Boolean = true
  override val isCommutative: Boolean = true
}
class TreeSize1Test extends AnyFunSuiteLike {
  test("Compute tree size") {

    val treeSize1: Module = {
      lazy val pat1 = Pattern(
        None,
        "Agg",
        Seq(Param("a", TScalaString), Param("b", TScalaString), Param("t", TScalaInt)),
        Seq(Body(Seq(ExtensionalCall("Coll", Seq(Var("a"), Var("b"), Var("t"))))))
      )

      lazy val pat2 = Pattern(
        None,
        "main",
        Seq(Param("b", TScalaInt)),
        Seq(Body(Seq(
          Computed(Var("b"), CustomAggregation(
            TScalaInt,
            None,
            Scala(q"new inca.backend.transform.monotype.sumAgg()"),
            "Agg",
            Seq(Var("m"), Var("z"), Var("k")),
            2
          ))
        )))
      )

      Module("treeSize1", Seq(), Seq(pat1, pat2), Seq())
    }

    val compiledModule = new CompiledModule {
      override val options: Options = new Options {
        override def optimizations: Seq[Optimization] = Seq()

        override def transformations: Seq[Transformation] = Seq(
        )

        override def stopOnError: Boolean = true

        override def stopOnWarning: Boolean = true

        override def withOptimizations(opts: Seq[Optimization]): Options = ???

        override def withTransformations(trans: Seq[Transformation]): Options = ???
      }

      override def name: Name = "treeSize1"

      override def sourceLocation: SourceLocation = ???

      override def ir: Module = treeSize1

      override def dataModel: DataModel = new DataModel()
    }

    val prog: DatalogAPI = new DatalogAPI(compiledModule)
    println(compiledModule.transformed)
    val edb: EDBChange = EDBChange.insertions(
      Seq(
        Relation3("Coll", Seq("a", "b", "t"), Seq(Seq("A", "B", 1), Seq("A", "C", 1))),
      )
    )
    prog.update(edb)
    println(prog.readAll)
    //    val res = prog.read(Relation2("main", Seq("m", "b"), Seq()))
    //    assert(res.size == 1)
    //    assert(res.toSet.toList.head == ("A", 5))
  }
}