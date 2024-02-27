package inca.casestudy.asg

import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.ir.*
import inca.ir.execution.{Relation2, Relation4}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.string.*
import inca.ir.util.SourceLocation
import inca.util.CSVUtil.{CSV, csvToString}
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.EnginePool

import java.io.IOException
import scala.language.implicitConversions

object AbstractSyntaxGraph:

  def t(s: String) = TData(s)
  def v(s: String) = Var(s)

  implicit def embed[A](a: A): Seq[A] = Seq(a)

  val datas = Seq(
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

  val edgesDefs = Relation("edgesDefs",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("from", t("TDef")),
      Param("to", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Call("edgesDef", Seq(v("defs"), v("hd"), v("from"), v("to")))
      )),
      Body(Seq(
        Deconstruct(v("defs"), "Cons", Seq(v("hd"), v("tl"))),
        Call("edgesDefs", Seq(v("tl"), v("from"), v("to")))
      ))
    )
  )
  val edgesDef = Relation("edgesDef",
    Seq(
      Param("defs", TDemand(t("TDefList"))),
      Param("def", TDemand(t("TDef"))),
      Param("from", t("TDef")),
      Param("to", t("TDef"))
    ),
    Seq(
      Body(Seq(
        Deconstruct(v("def"), "Def", Seq(v("tmp"), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("to"))),
        Eq(v("from"), v("def"))
      )),
      Body(Seq(
        Deconstruct(v("def"), "Def", Seq(v("tmp"), v("e"))),
        Call("target", Seq(v("defs"), v("e"), v("trg"))),
        Call("edgesDef", Seq(v("defs"), v("trg"), v("from"), v("to")))
      ))
    )
  )

  val target = Relation("target",
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

  val findDef = Relation("findDef",
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

  val makeLine = Relation("makeLine",
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

  val concat = Relation("concat",
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

  val main = Relation("main",
    Seq(
      Param("from", t("TDef")),
      Param("to", t("TDef"))
    ),
    Seq(
      Body(Seq(
        ExtensionalCall("input$main", Seq(v("endNode"), v("step"))),
        Call("makeProg", Seq(IntNum(0), v("endNode"), v("step"), v("defs"))),
        Call("edgesDefs", Seq(v("defs"), v("from"), v("to")))
      ))
    )
  )


  val mod = Module("AbstractSyntaxGraph", BaseIR.language + arithmetic.IR + data.IR + demand.IR + string.IR,
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

  def compiled = new CompiledModule:
    override def name: Name = "AbstractSyntaxGraph"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod
    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logLowerings = false
      opt.irLogging.logTypeInformation = false
      opt
    }
    setPipeline(List(() => new demand.Lowering {}))


  @main def benchmarkAsg() = {
    val resultPath = "benchmark/mono"
    val maxNodes = 100
    // Execution
    val measurements = for (i <- Range.inclusive(10, maxNodes, 10)) yield  {
      // Stats
      {
        val engine = new inca.viatra.Executor().instantiate(compiled)
        engine.insert(Relation2("input$main", Seq("endNode", "step"), Seq(Seq(i, 10))))
        val rels = engine.readAll()
        val stats = ("total" -> IndexedSeq(rels.map(_.size).sum.toLong)) +: engine.readAll().map { r =>
          r.name -> IndexedSeq(r.size.toLong)
        }
        FileUtil.writeFile(s"$resultPath/asg/ASG_DL_${i}_stats.csv", csvToString(toCSV(stats)))
      }

      collectGarbage()

      // Warmup
      for (k <- Range.inclusive(1, 5)) {
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

    FileUtil.writeFile(s"$resultPath/asg/ASG_DL.csv", csvToString(toCSV(measurements)))
  }

  @main def runAsgUsingSouffle() = {
    val engine = inca.souffle.backend.Executor.instantiate(compiled)
    engine.insert(Relation2("input$main", Seq("endNode", "step"), Seq(Seq(50, 10))))
    val rel = engine.read(Relation2("main", Seq("from", "to"), Seq()))
    println(rel.asTable)
  }

  