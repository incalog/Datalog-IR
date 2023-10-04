package inca.backend.transform.monotype

import inca.backend.hints.MagicSetHints

import collection.mutable
import scala.meta._
import inca.frontend.ir.{EDBChange, Relation1, Relation2, Relation3, Datalog => DatalogAPI}
import inca.backend.ir.Datalog.{AddMono, Atom, Body, Call, Computed, CustomAggregation, Eq, Evaluation, ExtensionalCall, IntConstant, MkMono, Module, Name, Param, Pattern, ResultMono, StringConstant, TScala, TScalaInt, TScalaString, Var}
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.util.Scala
import org.scalatest.funsuite.AnyFunSuiteLike
import inca.runtime.aggregate.MonoAggregation
import inca.runtime.context.DataModel


case class CountMono() extends MonoAggregation[Int, (String, Int), Int] {
  override val name: String = ""

  override def init: Int = 0

  override def add(st : Int, a : (String, Int)) : Int = st + 1

  override def result(st : Int) : Int = st
}

class MonoTransTest extends AnyFunSuiteLike {
//  // a = ("t", 1)
//  private lazy val bindPair : Atom = Computed(Var("a"), Evaluation(
//    Seq(),
//    TScala(Scala(t"(String, Int)")),
//    Scala(q"()=> (t, 1)")
//  ))

  private lazy val treeSize1: Module = {

    // size(t, m) :- leaf(t), m <- (t, 1).
    lazy val pat1Body1: Body = Body(Seq(
      ExtensionalCall("leaf", Seq(Var("t"))),
      Computed(Var("a"), Evaluation(
        Seq(Var("t") -> TScalaString),
        TScala(Scala(t"(String, Int)")),
        Scala(q"(t : String) => (t, 1)")
      )),
      AddMono(Var("m"), Var("a"))
    ))

    // size(t, m) :- btree(t, l, r), size(l, m), size(r, m), m <- (t, 1)
    lazy val pat1Body2: Body = Body(Seq(
      ExtensionalCall("btree", Seq(
        Var("t"),
        Var("l"),
        Var("r")
      )),
      Call("size", Seq(
        Var("l"),
        Var("m")
      )).addHint(MagicSetHints.FixedAdornment(Seq(true, true))),
      Call("size", Seq(
        Var("r"),
        Var("m")
      )).addHint(MagicSetHints.FixedAdornment(Seq(true, true))),
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
      Call("size", Seq(
        Var("t"),
        Var("m")
      )).addHint(MagicSetHints.FixedAdornment(Seq(true, true))),

      Eq(Var("t"), StringConstant("A")),
      ResultMono(Var("m"), Var("v")),
    ))

    lazy val pat1: Pattern = Pattern(
      None, "size",
      Seq(Param("t", TScalaString),
        Param("m", TScala(Scala(t"inca.backend.transform.monotype.CountMono")))),
      Seq(pat1Body1, pat1Body2)
    ).addHint(MagicSetHints.Main(Seq(true, true)))

    lazy val pat2: Pattern = Pattern(
      None, "main",
      Seq(Param("t", TScalaString),
        Param("v", TScalaInt)),
      Seq(pat2Body)
    ).addHint(MagicSetHints.Main(Seq(true, false)))


    Module(
      "treeSize1", Seq(), Seq(pat1, pat2), Seq()
    )
  }

  // tree size program after transformation
  private lazy val treeSize2 : Module = {
    // size(t, m) :- leaf(t), input_size(t, m)
    lazy val pat1Body1: Body = Body(Seq(
      ExtensionalCall("leaf", Seq(Var("t"))),
      Call("input_size", Seq(Var("t"), Var("m")))
    ))

    // size(t, m) :- btree(t, l, r), size(l, m), size(r, m), input_size(t, m)
    lazy val pat1Body2: Body = Body(Seq(
      ExtensionalCall("btree", Seq(
        Var("t"),
        Var("l"),
        Var("r")
      )),
      Call("size", Seq(
        Var("l"),
        Var("m")
      )),
      Call("size", Seq(
        Var("r"),
        Var("m")
      )),
      Call("input_size", Seq(Var("t"), Var("m")))
    ))

    lazy val pat1 : Pattern = Pattern(
      None,
      "size",
      Seq(Param("t", TScalaString), Param("m", TScala(Scala(t"inca.backend.transform.monotype.CountMono")))),
      Seq(pat1Body1, pat1Body2)
    )

    // main(t, b) :- size(t, m), t = "A", tmp = Agg(m, st), b = m.result(tmp)
    lazy val pat2Body: Body = Body(Seq(
      Call("size", Seq(
        Var("t"),
        Var("m")
      )),
      Eq(Var("t"), StringConstant("A")),
      Computed(Var("tmp"), CustomAggregation(
        TScalaInt,
        None,
        Scala(q"""new inca.backend.transform.monotype.CountMono()"""),
        "Coll",
        Seq(Var("m"), Var("st")),
        1
      )),
      Computed(Var("b"), Evaluation(
        Seq(
          Var("m") -> TScala(Scala(t"inca.backend.transform.monotype.CountMono")),
          Var("tmp") -> TScalaInt
        ),
        TScalaInt,
        Scala(q"(m : inca.backend.transform.monotype.CountMono, tmp: Int) => m.result(tmp)")
      ))
    ))

    lazy val pat2 : Pattern = Pattern(
      None,
      "main",
      Seq(Param("t", TScalaString), Param("b", TScalaInt)),
      Seq(pat2Body)
    )

    // input_size(l, m) :- btree(t, l, r), input_size(t, m)
    lazy val pat3Body1 : Body = Body(Seq(
      ExtensionalCall("btree", Seq(
        Var("t"), Var("l"), Var("r")
      )),
      Call("input_size", Seq(Var("t"), Var("m")))
    ))

    // input_size(l, m) :- btree(t, r, l), input_size(t, m)
    lazy val pat3Body2: Body = Body(Seq(
      ExtensionalCall("btree", Seq(
        Var("t"), Var("r"), Var("l")
      )),
      Call("input_size", Seq(Var("t"), Var("m")))
    ))

    // input_size(t, m) :- input(t, m)
    lazy val pat3Body3 : Body = Body(Seq(
      ExtensionalCall("input", Seq(Var("l"), Var("m")))
    ))

    lazy val pat3 : Pattern = Pattern(
      None,
      "input_size",
      Seq(Param("l", TScalaString), Param("m", TScala(Scala(t"inca.backend.transform.monotype.CountMono")))),
      Seq(pat3Body1, pat3Body2, pat3Body3)
    )

    // Coll(m, v) :- leaf(t), v = (t, 1), input_size(t, m)
    lazy val pat4Body1 : Body = Body(Seq(
      ExtensionalCall("leaf", Seq(Var("t"))),
      Computed(Var("v"), Evaluation(
        Seq(Var("t") -> TScalaString),
        TScala(Scala(t"(String, Int)")),
        Scala(q"(t : String) => (t, 1)")
      )),
      Call("input_size", Seq(Var("t"), Var("m")))
    ))

    // Coll(m, v) :- btree(t, l, r), v = (t, 1), size(l, m),
    //               size(r, m), inputSize(t, m)
    lazy val pat4Body2 : Body = Body(Seq(
      ExtensionalCall("btree", Seq(Var("t"), Var("l"), Var("r"))),
      Computed(Var("v"), Evaluation(
        Seq(Var("t") -> TScalaString),
        TScala(Scala(t"(String, Int)")),
        Scala(q"(t : String) => (t, 1)")
      )),
      Call("size", Seq(Var("l"), Var("m"))),
      Call("size", Seq(Var("r"), Var("m"))),
      Call("input_size", Seq(Var("t"), Var("m")))
    ))

    lazy val pat4 : Pattern = Pattern(
      None,
      "Coll",
      Seq(Param("m", TScala(Scala(t"inca.backend.transform.monotype.CountMono"))), Param("v", TScala(Scala(t"(String, Int)")))),
      Seq(pat4Body1, pat4Body2)
    )

    Module("Test", Seq(), Seq(pat1, pat2, pat3, pat4), Seq())
  }

  lazy val compiledTreeSizeModule : CompiledModule = new CompiledModule {
    override val options: Options = new Options {
      override def optimizations: Seq[Optimization] = Seq()

      override def transformations: Seq[Transformation] = Seq()

      override def stopOnError: Boolean = true

      override def stopOnWarning: Boolean = true

      override def withOptimizations(opts: Seq[Optimization]): Options = ???

      override def withTransformations(trans: Seq[Transformation]): Options = ???
    }

    override def name: Name = "Test"

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    override def ir: Module = treeSize2

    override def dataModel: DataModel = new DataModel()
  }

  val m: CountMono = CountMono()

  val edb : EDBChange = EDBChange.insertions(
    Seq(
      Relation3("btree", Seq("t", "l", "r"), Seq(Seq("A", "B", "C"), Seq("C", "D", "E"))),
      Relation1("leaf", Seq("t"), Seq(Seq("B"), Seq("D"), Seq("E"))),
      Relation2("input", Seq("node", "mono"), Seq(Seq("A", m)))
    )
  )

  test("Tree size") {
    val treeSizeDatalog : DatalogAPI = new DatalogAPI(compiledTreeSizeModule)
    treeSizeDatalog.update(edb)
    println(treeSizeDatalog.readAll)
  }

  // test the use of demand transformation
  val treeSize3 : Module = {
    // size(t, v) :- leaf(t), v = 1
    // size(t, v) :- btree(t, l, r), size(l, v1), size(r, v2), v = v1 + v2
    // main(t, v) :- size(t, v)
    // mark `t` as bound, `v` as free
    val pat1Body1 : Body = Body(Seq(
      ExtensionalCall("leaf", Seq(Var("t"))),
      Eq(Var("v"), IntConstant(1))
    ))

    val pat1Body2 : Body = Body(Seq(
      ExtensionalCall("btree", Seq(Var("t"), Var("l"), Var("r"))),
      Call("size", Seq(Var("l"), Var("v1"))),
      Call("size", Seq(Var("r"), Var("v2"))),
      Computed(Var("v"), Evaluation(
        Seq(Var("v1") -> TScalaInt, Var("v2") -> TScalaInt),
        TScalaInt,
        Scala(q"(v1 : Int, v2 : Int) => v1 + v2")
      ))
    ))

    val pat1 : Pattern = Pattern(
      None,
      "size",
      Seq(Param("t", TScalaString), Param("v", TScalaInt)),
      Seq(pat1Body1, pat1Body2)
    ).addHint(MagicSetHints.FixedAdornment(Seq(true, false)))

    val pat2 : Pattern = Pattern(
      None,
      "main",
      Seq(Param("t", TScalaString), Param("v", TScalaInt)),
      Seq(Body(Seq(Call("size", Seq(Var("t"), Var("v"))))))
    ).addHint(MagicSetHints.Main(Seq(true, false)))

    Module(
      "treeSize",
      Seq(),
      Seq(pat1, pat2),
      Seq()
    )
  }
  lazy val demandModule : CompiledModule = new CompiledModule {
    override val options: Options = new Options {
      override def optimizations: Seq[Optimization] = Seq()

      override def transformations: Seq[Transformation] = Seq(
        DeriveDemandPatterns,
        DemandTransformation
      )

      override def stopOnError: Boolean = true

      override def stopOnWarning: Boolean = true

      override def withOptimizations(opts: Seq[Optimization]): Options = ???

      override def withTransformations(trans: Seq[Transformation]): Options = ???
    }

    override def name: Name = "treeSize"

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    override def ir: Module = treeSize3

    override def dataModel: DataModel = new DataModel()
  }


  test("Demand Transformation"){
    val prog : DatalogAPI = new DatalogAPI(demandModule)
    println(demandModule.transformed)
    val edb1: EDBChange = EDBChange.insertions(
      Seq(
        Relation3("btree", Seq("t", "l", "r"), Seq(Seq("A", "B", "C"), Seq("C", "D", "E"))),
        Relation1("leaf", Seq("t"), Seq(Seq("B"), Seq("D"), Seq("E"))),
        Relation1("ext_input$main$bf", Seq("t$0"), Seq(Seq("A")))
      )
    )
    prog.update(edb1)
    println(prog.readAll)
  }

  val treeSize1Module = new CompiledModule {
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

    override def name: Name = "treeSize1"

    override def sourceLocation: SourceLocation = ???

    override def ir: Module = treeSize1

    override def dataModel: DataModel = new DataModel()
  }

  test("Transform MonoCount") {
    val prog: DatalogAPI = new DatalogAPI(treeSize1Module)
    println(treeSize1Module.transformed)
    val edb1: EDBChange = EDBChange.insertions(
      Seq(
        Relation3("btree", Seq("t", "l", "r"), Seq(Seq("A", "B", "C"), Seq("C", "D", "E"))),
        Relation1("leaf", Seq("t"), Seq(Seq("B"), Seq("D"), Seq("E"))),
        Relation1("ext_input$main$bf", Seq("t$0"), Seq(Seq("A"))),
        Relation2("ext_input$size$bb", Seq("t", "m"), Seq(Seq("A", CountMono())))
      )
    )
    prog.update(edb1)
    println(prog.readAll)
  }
}
