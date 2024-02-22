package inca.frontend.oodl.casestudy

import inca.foreign.scala.ir.primitive.{ConversionElimination, ScalaMonoDefinition}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.{arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.execution.{Relation2, Relation4}
import inca.ir.extension.arithmetic.{Add, GE, IntNum, LT, TInt}
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, ExtensionalCall, ExtensionalRelation, Module, Name, Param, Relation, Var, WildcardArg, string2name, term2Arg}
import inca.ir.extension.{arithmetic, block, data, demand, impure, mono, not, string, aggregate as incaAgg, bool as incaBool, disjunction as incaDisj, set as incaSet, tuple as incaTuple}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuiteLike
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.extension.set.{SetMember, TSet}
import inca.ir.extension.mono.{MonoImpurityKind, MonoTypes, NewMono, ReadMono, SetMonoDefinition, TMono, WriteMono}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.impure.{Impure, MainHint}
import inca.ir.extension.string.{StringConcat, StringLit, TString, ToString}
import inca.ir.typing.{BaseIRTypechecker, DependencyInfo, IRTypechecker}
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.util.CSVUtil.{CSV, csvToString}
import inca.util.FileUtil

import java.io.IOException
import scala.language.implicitConversions

class AbstractSyntaxGraphMono extends AnyFunSuiteLike:

  implicit def embed[A](a: A): Seq[A] = Seq(a)

  def t(s: String) = TData(s)
  def v(s: String) = Var(s)

  def tEdgePair = TTuple(Seq(t("TDef"), t("TDef")))
  def tEdgeSetMono = TMono(tEdgePair, TSet(tEdgePair), Seq())

  def datas = Seq(
    DataDefinition("TProg"),
    CaseDefinition("Prog", t("TDefList"), t("TProg")),

    DataDefinition("TDefList"),
    CaseDefinition("Nil", Seq(), t("TDefList")),
    CaseDefinition("Cons", Seq(t("TDef"), t("TDefList")), t("TDefList")),

    DataDefinition("TDef"),
    CaseDefinition("Def", Seq(TString, t("TExp")), t("TDef")),

    DataDefinition("TExp"),
    CaseDefinition("Num", TInt, t("TExp")),
    CaseDefinition("Var", TString, t("TExp")),
    CaseDefinition("Add", Seq(t("TExp"), t("TExp")), t("TExp")),
  )

  def edgesDefs = Relation("edgesDefs",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("mono", TDemand(tEdgeSetMono))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Call("edgesDef", Seq(v("defs"), v("hd"), v("mono")))
      )),
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Call("edgesDefs", Seq(v("tl"), v("mono")))
      ))
    )
  )
  def edgesDef = Relation("edgesDef",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("def", TDemand(t("TDef"))),
      Param("mono", TDemand(tEdgeSetMono))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("def"), "Def", Seq(v("tmp"), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("to"))),
        Eq(v("from"), v("def")),
      )),
      Body(Seq(
        Deconstruct(v("def"), "Def", Seq(v("tmp"), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("trg"))),
        WriteMono(Var("mono"), TupleLit(Seq(Var("def"), Var("trg")))),
        Call("edgesDef", Seq(v("defs"), v("trg"), v("mono")))
      ))
    )
  )

  def target = Relation("target",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("e", TDemand(t("TExp"))),
      Param("def", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("e"), "Var", Seq(v("name"))),
        Call("findDef", Seq(v("defs"), v("name"), v("def")))
      )),
      Body(Seq(
        Deconstruct(v("e"), "Add", Seq(v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e1"), v("def")))
      )),
      Body(Seq(
        Deconstruct(v("e"), "Add", Seq(v("e1"), v("e2"))),
        Call("target", Seq(v("defs"), v("e2"), v("def")))
      ))
    )
  )

  def findDef = Relation("findDef",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("name", TDemand(TString)),
      Param("def", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Deconstruct(v("hd"), "Def", Seq(v("defname"), v("tmp"))),
        Eq(v("defname"), v("name")),
        Eq(v("def"), v("hd"))
      )),
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Deconstruct(v("hd"), "Def", Seq(v("defname"), v("tmp"))),
        Eq(v("defname"), v("name"), neg = true),
        Call("findDef", Seq(v("tl"), v("name"), v("def")))
      ))
    )
  )

  val makeProg = Relation("makeProg",
    Seq(
      Param("from", TDemand(TInt)),
      Param("to", TDemand(TInt)),
      Param("step", TDemand(TInt)),
      Param("defs", t("TDefList"))
    ),
    Seq(
      Body(Seq(
        LT(v("from"), v("to")),
        Eq(v("next"), Add(v("from"), v("step"))),
        Call("makeLine", Seq(v("from"), v("next"), v("line"))),
        Eq(v("circle"), Construct("Def", Seq(
          StringConcat(StringLit("a"), ToString(v("from"))),
          Construct("Add", Seq(
            Construct("Var", StringConcat(StringLit("a"), ToString(v("next")))),
            Construct("Num", v("from"))
          ))
        ))),
        Call("makeProg", Seq(v("next"), v("to"), v("step"), v("rec"))),
        Call("concat", Seq(
          v("line"),
          Construct("Cons", Seq(v("circle"), v("rec"))),
          v("defs")
        ))
      )),
      Body(Seq(
        GE(v("from"), v("to")),
        Eq(v("circle"), Construct("Def", Seq(
          StringConcat(StringLit("a"), ToString(v("from"))),
          Construct("Add", Seq(
            Construct("Var", StringConcat(StringLit("a"), ToString(IntNum(0)))),
            Construct("Num", v("from"))
          ))
        ))),
        Eq(v("defs"), Construct("Cons", Seq(v("circle"), Construct("Nil", Seq()))))
      ))
    )
  )

  def makeLine = Relation("makeLine",
    Seq(
      Param("i", TDemand(TInt)),
      Param("to", TDemand(TInt)),
      Param("defs", t("TDefList"))
    ),
    Seq(
      Body(Seq(
        LT(v("i"), v("to")),
        Eq(v("n"), Add(v("i"), IntNum(1))),
        Eq(v("d"), Construct("Def", Seq(
          StringConcat(StringLit("a"), ToString(v("i"))),
          Construct("Add", Seq(
            Construct("Var", StringConcat(StringLit("a"), ToString(v("n")))),
            Construct("Num", v("i"))
          ))
        ))),
        Call("makeLine", Seq(v("n"), v("to"), v("ds"))),
        Eq(v("defs"), Construct("Cons", Seq(v("d"), v("ds"))))
      )),
      Body(Seq(
        GE(v("i"), v("to")),
        Eq(v("defs"), Construct("Nil", Seq()))
      ))
    )
  )

  def concat = Relation("concat",
    Seq(
      Param("l1", TDemand(t("TDefList"))),
      Param("l2", TDemand(t("TDefList"))),
      Param("res", t("TDefList"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("l1"), "Nil", Seq()),
        Eq(v("res"), v("l2"))
      )),
      Body(Seq(
        Deconstruct(v("l1"), "Cons", Seq(v("hd"), v("tl"))),
        Call("concat", Seq(v("tl"), v("l2"), v("tmp"))),
        Eq(v("res"), Construct("Cons", Seq(v("hd"), v("tmp"))))
      ))
    )
  )

  val inputMain = ExtensionalRelation("input$main", Seq(Param("endNode", TInt), Param("step", TInt)))

  def main = Relation("main",
    Seq(
      Param("from", t("TDef")),
      Param("to", t("TDef"))
    ),
    Seq(
      Body(Seq(
        ExtensionalCall("input$main", Seq(v("endNode"), v("step"))),
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Call("makeProg", Seq(IntNum(0), v("endNode"), v("step"), v("defs"))),
        Eq(v("mono"), NewMono(SetMonoDefinition(tEdgePair), Seq(), Seq())),
        Call("edgesDefs", Seq(v("defs"), v("mono"))),
        //        SetMember(TupleLit(Seq(v("from"), v("to"))), ReadMono(v("mono")))
        SetMember(Var("v"), ReadMono(v("mono"))),
        Eq(Var("from"), Project(Var("v"), 0)),
        Eq(Var("to"), Project(Var("v"), 1)),
      ))
    )
  ).addHint(MainHint)


  private def mod = Module("AbstractSyntaxGraph", BaseIR.language + arithmetic.IR + data.IR + demand.IR + mono.IR + incaSet.IR + string.IR + impure.IR + incaTuple.IR + incaAgg.IR + incaBool.IR,
    datas ++
      Seq(
        edgesDefs,
        edgesDef,
        target,
        findDef,
        makeProg,
        makeLine,
        concat,
        main,
        inputMain
      )
  )

  class Compiled(optMono: Boolean) extends CompiledModule:
    override def name: Name = "AbstractSyntaxGraph"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override val ir: Module = mod
    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logLowerings = false
      opt.irLogging.logTypeInformation = false
      opt
    }
    override def typechecker: BaseIRTypechecker = new IRTypechecker with primitive.Typechecker
    private trait demandLowering extends demand.Lowering with primitive.Visitor
    private trait blockLowering extends block.Lowering with primitive.Visitor
    private trait scalaLowering extends primitive.ScalaLowering
      with scalaArith.ScalaLowering
      with scalaData.ScalaLowering
      with scalaString.ScalaLowering

    setPipeline(List(
      () => new mono.Lowering(optMono) {},
      () => new MonoScalaLowering {},
      () => new ConversionElimination {},
      () => new impure.Lowering {},
      () => new demand.Lowering {},
      () => new incaBool.Lowering {},
      () => new blockLowering {},
      () => new incaSet.Lowering {},
      () => new incaTuple.Lowering {},
      () => new blockLowering {},
      () => new incaDisj.Lowering {},
      () => new not.Lowering {},
      () => new demandLowering {},
    ))

  test("AbstractSyntaxGraph is well-typed: Set Mono Aggregation") {
    val compiled = new Compiled(false)
    try compiled.checked
    //finally println(compiled.ir)
  }

  test("AbstractSyntaxGraph can be lowered without optimization: Set Mono Aggregation") {
    new Compiled(false).lowered
  }

  test("AbstractSyntaxGraph can be lowered with optimization: Set Mono Aggregation") {
    new Compiled(true).lowered
  }

  test("AbstractSyntaxGraph can be run without optimization: Set Mono Aggregation") {
    for (i <- 0 until 5) {
      val compiled = new Compiled(false)

//      println(compiled.dependencyGraph.toGraphViz)
//      val check = new IRTypechecker with primitive.Typechecker
//      check.checkProgram(Seq(compiled.lowered))
//      val graph = check.getDependencyGraph
//      println(graph.filter(_ => true, (_, _, info) => info != DependencyInfo.TypeReference).toGraphViz)
//      graph.cycles.foreach(c => println(graph.prettyPrintCycle(c)))

      val engine = new inca.viatra.Executor().instantiate(compiled)
      val start = System.currentTimeMillis()
      val relation1 = engine.read(Relation2("main", Seq("from", "to"), Seq()))
//      val relation2 = engine.read(Relation4("makeProg", Seq("from", "to", "step", "defs"), Seq()))
      val end = System.currentTimeMillis()
      //println(s"Execution time ${end - start}ms")
      //println(relation1.asTable)
    }
  }

  private def toCSV(vals: Seq[(String, IndexedSeq[Long])]): CSV = {
    val header = vals.map(_._1).toIndexedSeq
    // we assume that each list has same number of elements
    val rowLength = vals.head._2.size
    val rows = for (i <- 0 until rowLength) yield vals.map(_._2(i)).toIndexedSeq
    header +: rows
  }

  private def collectGarbage(): Unit = {
    System.gc()
    try {
      Thread.sleep(2000)
    } catch {
      case e: IOException => e.printStackTrace()
    }
  }

  test("Measure: AbstractSyntaxGraphMono") {
    val maxNodes = 10
    val resultPath = "benchmark/mono"


    for (opt <- Seq(false, true)) {
      val suffix = if opt then "_opt" else ""

      val compiled = new Compiled(opt)
      // Execution
      val measurements = for (i <- Range.inclusive(10, maxNodes, 10)) yield {
        // Stats
        {
          val engine = new inca.viatra.Executor().instantiate(compiled)
          engine.insert(Relation2("input$main", Seq("endNode", "step"), Seq(Seq(i, 10))))
          val rels = engine.readAll()
          val stats = ("total" -> IndexedSeq(rels.map(_.size).sum.toLong)) +: engine.readAll().map { r =>
            r.name -> IndexedSeq(r.size.toLong)
          }
          FileUtil.writeFile(s"$resultPath/asg/ASG_Mono${suffix}_${i}_stats.csv", csvToString(toCSV(stats)))
        }

        collectGarbage()

        // Warmup
        for (k <- 2) {
          val engine = new inca.viatra.Executor().instantiate(compiled)
          engine.insert(Relation2("input$main", Seq("endNode", "step"), Seq(Seq(i, 10))))
          val relation1 = engine.read(Relation2("main", Seq("from", "to"), Seq()))
        }

        i.toString -> (for (j <- Range.inclusive(1, 5)) yield {
          val engine = new inca.viatra.Executor().instantiate(compiled)
          engine.insert(Relation2("input$main", Seq("endNode", "step"), Seq(Seq(i, 10))))
          val diff = engine.measure(Relation2("main", Seq("from", "to"), Seq()))
          collectGarbage()
          diff
        })
      }

      FileUtil.writeFile(s"$resultPath/asg/ASG_Mono$suffix.csv", csvToString(toCSV(measurements)))
    }
  }

  test("AbstractSyntaxGraph can be run with optimization: Set Mono Aggregation - Souffle") {
    val compiled = new Compiled(true)

    val engine = inca.souffle.backend.Executor.instantiate(compiled)
    val start = System.currentTimeMillis()
    val relation1 = engine.read(Relation2("main", Seq("from", "to"), Seq()))
    //      val relation2 = engine.read(Relation4("makeProg", Seq("from", "to", "step", "defs"), Seq()))
    val end = System.currentTimeMillis()

    //println(s"Execution time: ${end - start}ms")
    //println(relation1.asTable)

    assertResult(57)(relation1.entries.size)
  }
