package inca.casestudy.interval

import inca.casestudy.interval
import inca.casestudy.interval.Benchmark.nestedWhileProgram
import inca.casestudy.interval.IntervalAnalysisMono.{TAssign, TExit}
import inca.casestudy.interval.edb.{Assign, Num, Sequence, While}
import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.foreign.scala.ir.primitive.ScalaInca.cleanString
import inca.foreign.scala.ir.primitive.{IR, Typechecker, *}
import inca.foreign.scala.ir.{primitive, arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4}
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.{IR as dataIR, *}
import inca.ir.extension.demand.*
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.edbdata.*
import inca.ir.extension.edbdata.Link.Parent
import inca.ir.extension.impure.Impure
import inca.ir.extension.map.{MapComprehension, MapLookUp, TMap, IR as mapIR}
import inca.ir.extension.string.*
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.ir.{Body, Term, string2name, term2Arg, *}
import inca.util.CSVUtil.csvToString
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.Executor
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.DataModel
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory

import scala.language.implicitConversions

// Example used in our Paper: "Mono Types — First-Class Containers for Datalog" section 5.2
object IntervalAnalysis:

  def q(name: String): String = s"inca.casestudy.interval.edb.$name"

  val edbNodes = EdbDataModuleEntry.fromNodeMetaInfos(edb.allNodes)

  val TStmt = TEdbNode(q("Stmt"))
  val TSkip = TEdbNode(q("Skip"))
  val TSequence = TEdbNode(q("Sequence"))
  val TAssign = TEdbNode(q("Assign"))
  val TWhile = TEdbNode(q("While"))
  val TExit: TEdbNode = TEdbNode(q("Exit"))
  val TExp = TEdbNode(q("Exp"))
  val TVar = TEdbNode(q("Var"))
  val TNum = TEdbNode(q("Num"))
  val TAdd = TEdbNode(q("Add"))
  val TGT = TEdbNode(q("GT"))
  val TInterval = TData("Interval")

  val dataDefs = Seq(
    DataDefinition("Interval"),
    CaseDefinition("IV", Seq(TInt, TInt), TInterval),
    CaseDefinition("Top", Seq(), TInterval),
    CaseDefinition("Bot", Seq(), TInterval),
    CaseDefinition("BTrue", Seq(), TInterval),
    CaseDefinition("BFalse", Seq(), TInterval),
  )

  val dataModel: DataModel = DataModel.from(edb.allNodes*)

  def mkIv(l: Term, r: Term): Term = Construct("IV", Seq(l, r))

  val initStmt = Relation("initStmt", Seq(
    Param("stmt", TStmt),
    Param("out", TStmt)
  ), Seq(
    Body(Seq(
      //EdbDeconstruct(Var("stmt"), "Sequence", "s1" -> Var("s1")),
      Eq(Var("stmt"), LookupEdbType(TSequence)),
      Eq(Var("s1"), LookupEdbField(Cast(Var("stmt"), TSequence), "s1")),
      Call("initStmt", Seq(Var("s1"), Var("out")))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TSkip)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TWhile)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TExit)),
      Eq(Var("out"), Var("stmt"))
    )),
  ))

  val finalStmt = Relation("finalStmt", Seq(
    Param("stmt", TStmt),
    Param("out", TStmt)
  ), Seq(
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TSequence)),
      Eq(Var("s2"), LookupEdbField(Cast(Var("stmt"), TSequence), "s2")),
      Call("finalStmt", Seq(Var("s2"), Var("out")))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TSkip)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TAssign)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TWhile)),
      Eq(Var("out"), Var("stmt"))
    )),
    Body(Seq(
      Eq(Var("stmt"), LookupEdbType(TExit)),
      Eq(Var("out"), Var("stmt"))
    )),
  ))

  val cflow = Relation("cflow", Seq(
    Param("from", TStmt),
    Param("to", TStmt)
  ), Seq(
    Body(
      Seq(
        Eq(Var("seqs"), LookupEdbType(TSequence)),
        Eq(Var("s1"), LookupEdbField(Cast(Var("seqs"), TSequence), "s1")),
        Eq(Var("s2"), LookupEdbField(Cast(Var("seqs"), TSequence), "s2")),
        Call("finalStmt", Seq(Var("s1"), Var("from"))),
        Call("initStmt", Seq(Var("s2"), Var("to"))),
      )
    ),
    Body(
      Seq(
        Eq(Var("whl"), LookupEdbType(TWhile)),
        Eq(Var("cond"), LookupEdbField(Cast(Var("whl"), TWhile), "cond")),
        Eq(Var("body"), LookupEdbField(Cast(Var("whl"), TWhile), "body")),
        Eq(Var("whl"), Var("from")),
        Call("initStmt", Seq(Var("body"), Var("to"))),
      )
    ),
    Body(
      Seq(
        Eq(Var("whl"), LookupEdbType(TWhile)),
        Eq(Var("cond"), LookupEdbField(Cast(Var("whl"), TWhile), "cond")),
        Eq(Var("body"), LookupEdbField(Cast(Var("whl"), TWhile), "body")),
        Call("finalStmt", Seq(Var("body"), Var("from"))),
        Eq(Var("whl"), Var("to"))
      )
    )
  ))


  val allVars = Relation("allVars", Seq(
    Param("name", TString)
  ), Seq(
    Body(Seq(
      Eq(Var("s"), LookupEdbType(TAssign)),
      Eq(Var("_name"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
      Eq(Var("name"), Cast(Var("_name"), TString))
    ))
  ))

  val parentOf = Relation("parentOf", Seq(
    Param("exp", TExp),
    Param("stmt", TStmt),
  ), Seq(
    Body(Seq(
      Eq(Var("exp"), LookupEdbType(TExp)),
      Eq(Var("_p"), LookupEdbField(Var("exp"), Parent)),
      Eq(Cast(Var("_p"), TStmt), LookupEdbType(TStmt)),
      Eq(Var("stmt"), Cast(Var("_p"), TStmt)),
    )),
    Body(Seq(
      Eq(Var("exp"), LookupEdbType(TExp)),
      Eq(Var("_p"), LookupEdbField(Var("exp"), Parent)),
      Eq(Cast(Var("_p"), TExp), LookupEdbType(TExp)),
      Call("parentOf", Seq(Cast(Var("_p"), TExp), Var("stmt")))
    ))
  ))

  val aeval = Relation("aeval",
    Seq(
      Param("stmt", TStmt),
      Param("exp", TExp),
      Param("iv", TInterval)
    ),
    Seq(
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TVar)),
        Call("parentOf", Seq(Var("exp"), Var("stmt"))),
        Eq(Var("_name"), LookupEdbField(Cast(Var("exp"), TVar), "name")),
        Eq(Var("name"), Cast(Var("_name"), TString)),
        Call("intervalBefore", Seq(Var("stmt"), Var("name"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TNum)),
        Call("parentOf", Seq(Var("exp"), Var("stmt"))),
        Eq(Var("n"), LookupEdbField(Cast(Var("exp"), TNum), "value")),
        Eq(Var("iv"), mkIv(Cast(Var("n"), TInt), Cast(Var("n"), TInt)))
      )),
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TAdd)),
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TAdd), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TAdd), "rhs")),
        Call("aeval", Seq(Var("stmt"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("stmt"), Var("rhs"), Var("iv2"))),
        Disjunction(Seq(
          DisjunctionAlternative(
            Deconstruct(Var("iv1"), "IV", Seq(Var("l1"), Var("l2"))),
            Deconstruct(Var("iv2"), "IV", Seq(Var("l3"), Var("l4"))),
            Eq(Var("iv"), mkIv(Add(Var("l1"), Var("l3")), Add(Var("l2"), Var("l4"))))
          ),
          DisjunctionAlternative(
            Deconstruct(Var("iv1"), "Top", Seq()),
            Eq(Var("iv"), Construct("Top", Seq()))
          ),
          DisjunctionAlternative(
            Deconstruct(Var("iv2"), "Top", Seq()),
            Eq(Var("iv"), Construct("Top", Seq()))
          ),
        )),
      )),
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TGT)),
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TGT), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TGT), "rhs")),
        Call("aeval", Seq(Var("stmt"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("stmt"), Var("rhs"), Var("iv2"))),
        Deconstruct(Var("iv1"), "IV", Seq(Var("l1"), Var("h1"))),
        Deconstruct(Var("iv2"), "IV", Seq(Var("l2"), Var("h2"))),
        Disjunction(Seq(
          DisjunctionAlternative(
            GT(Var("l1"), Var("h2")),
            Eq(Var("iv"), Construct("BTrue", Seq()))
          ),
          DisjunctionAlternative(
            GT(Var("l2"), Var("h1")),
            Eq(Var("iv"), Construct("BFalse", Seq()))
          ),
          DisjunctionAlternative(
            LE(Var("l1"), Var("h2")),
            LE(Var("l2"), Var("h1")),
            Eq(Var("iv"), Construct("Top", Seq()))
          ),
        ))
      )),
      Body(Seq(
        Eq(Var("exp"), LookupEdbType(TGT)),
        Eq(Var("lhs"), LookupEdbField(Cast(Var("exp"), TGT), "lhs")),
        Eq(Var("rhs"), LookupEdbField(Cast(Var("exp"), TGT), "rhs")),
        Call("aeval", Seq(Var("stmt"), Var("lhs"), Var("iv1"))),
        Call("aeval", Seq(Var("stmt"), Var("rhs"), Var("iv2"))),
        Disjunction(Seq(
          DisjunctionAlternative(
            Deconstruct(Var("iv1"), "Top", Seq()),
          ),
          DisjunctionAlternative(
            Deconstruct(Var("iv2"), "Top", Seq()),
          ),
        )),
        Eq(Var("iv"), Construct("Top", Seq())),
      )),
    )
  )

  val intervalOp = ScalaAggregationOperator(
    "JoinInterval",
    ScalaType("Interval"),
    "Bot()",
    addCode =
      """(st: Interval, a: Interval) => (st, a) match {
        |    case (Bot(), _) => a
        |    case (_,Bot()) => st
        |    case (Top(), _) => Top()
        |    case (_, Top()) => Top()
        |    case (BTrue(), BTrue()) => BTrue()
        |    case (BFalse(), BFalse()) => BFalse()
        |    case (IV(l1, l2), IV(l3, l4)) =>
        |      val l = l1.min(l3)
        |      val h = l2.max(l4)
        |      if ((h - l).abs <= 5) then IV(l, h) else Top()
        |    case _ => Top()
        |}""".stripMargin,
  )

  val _intervalAfter = Relation("_intervalAfter",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TAssign)),
        Eq(Var("_v"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
        Eq(Var("v"), Cast(Var("_v"), TString)),
        Eq(Var("e"), LookupEdbField(Cast(Var("s"), TAssign), "exp")),
        Call("aeval", Seq(Var("stmt"), Var("e"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TAssign)),
        Eq(Var("_name"), LookupEdbField(Cast(Var("s"), TAssign), "name")),
        Eq(Var("name"), Cast(Var("_name"), TString)),
        Call("allVars", Seq(Var("v"))),
        Eq(Var("v"), Var("name"), true),
        Call("intervalBefore", Seq(Var("stmt"), Var("v"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TSkip)),
        Call("intervalBefore", Seq(Var("stmt"), Var("v"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TWhile)),
        Call("intervalBefore", Seq(Var("stmt"), Var("v"), Var("iv")))
      )),
      Body(Seq(
        Eq(Var("stmt"), LookupEdbType(TExit)),
        Call("intervalBefore", Seq(Var("stmt"), Var("v"), Var("iv")))
      ))
    )
  )

  /*val intervalAfter = Relation("intervalAfter",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        Call("_intervalAfter", Seq(Var("stmt").arg, Var("v").arg, WildcardArg())),
        Aggregate(RefByName("_intervalAfter"), Seq(Var("stmt").arg, Var("v").arg, AggregateColumnArg(Var("_iv"))), intervalOp),
        Eq(Var("iv"), Cast(Var("_iv"), TInterval))
      ))
    ))*/

  val predecessorIntervals = Relation("predecessorIntervals",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("pred", TStmt),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        Call("cflow", Seq(Var("pred"), Var("stmt"))),
        Call("_intervalAfter", Seq(Var("pred"), Var("v"), Var("iv")))
      ))
    )
  )

  val intervalBefore = Relation("intervalBefore",
    Seq(
      Param("stmt", TStmt),
      Param("v", TString),
      Param("iv", TInterval),
    ),
    Seq(
      Body(Seq(
        //Eq(Var("stmt"), LookupEdbType(TStmt)),
        //Call("allVars", Seq(Var("v"))),
        Call("predecessorIntervals", Seq(Var("stmt").arg, Var("v").arg, WildcardArg(), WildcardArg())),
        Aggregate(RefByName("predecessorIntervals"), Seq(Var("stmt").arg, Var("v").arg, WildcardArg(), AggregateColumnArg(Var("_iv"))), intervalOp),
        Eq(Var("iv"), Cast(Var("_iv"), TInterval))
      ))
    )
  )
  val main = Relation("main", Seq(
    Param("exit", TStmt),
    Param("x", TString),
    Param("x_iv", TInterval)
  ), Seq(
    Body(Seq(
      // output
      Eq(Var("exit"), LookupEdbType(TExit)),
      // Comment this in to verify that we only read from one map. Is it the double aggregation bug again ?
      Call("allVars", Seq(Var("x"))),
      Call("_intervalAfter", Seq(Var("exit"), Var("x"), Var("x_iv")))
    ))
  ))


  val mod = Module("IntervalAnalysis", BaseIR.language + arithmetic.IR + dataIR + mapIR + demand.IR + string.IR + edbdata.IR + disjunction.IR,
    edbNodes ++ dataDefs ++ Seq(
      allVars,
      parentOf,
      cflow,
      initStmt,
      finalStmt,
      aeval,
      predecessorIntervals,
      //intervalAfter,
      _intervalAfter,
      intervalBefore,
      main
    )
  )


  def compiled = new CompiledUnit:
    override def name: Name = "IntervalAnalysis"

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    override val irModules: Seq[Module] = Seq(mod)

    override val isClosedWorld: Boolean = true

    override val otherUnits: Seq[CompiledUnit] = Seq()

    override def compilerOptions: CompilerOptions =
      val op = CompilerOptions.default
      op.irLogging.logModule = false
      op.irLogging.logLowerings = false
      op

    private trait demandLowering extends demand.Lowering with primitive.Visitor

    private trait blockLowering extends block.Lowering with primitive.Visitor

    private trait scalaLowering extends primitive.ScalaLowering
      with scalaArith.ScalaLowering
      with scalaData.ScalaLowering
      with scalaString.ScalaLowering

    override def typechecker = new IRTypechecker with Typechecker {}

    setPipeline(List(
      () => new disjunction.Lowering {}
    ))

  private def run(prog: edb.Stmt): Any =
    try
      compiled.checked
    val exec = new Executor(DRedReteBackendFactory.INSTANCE)
    val engine = exec.instantiate(compiled, dataModel)

    val startLoad = System.nanoTime()
    engine.feed.processEditScript(prog.loadEdits)
    val endLoad = System.nanoTime()
    val loadTimeMs = (endLoad - startLoad) / 1000000

    val startProp = System.nanoTime()
    val spec = engine.module.patterns(cleanString("main"))()
    val matcher = spec.getMatcher(engine.engine)
    val endProp = System.nanoTime()
    val propTimeMs = (endProp - startProp) / 1000000


    val mainRel = engine.read(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))
    println(mainRel.asTable)
    val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))
    println(s"${cflowRel.size} cflow entries")

    println(s"Load time ${loadTimeMs}ms")
    println(s"Propagation time ${propTimeMs}ms")
  //    println(prog.toStringWithURI)


  @main def measureBigWhile = {
    val resultPath = "benchmark/mono"

    val warmups = 3
    val runs = 5

    val start = 10
    val maxRep = 50
    val step = 5
    val nestings = 5

    val measurements = for (reps <- Range.inclusive(start, maxRep, step)) yield {
      // input program
      val s = nestedWhileProgram(nestings, reps)

      var cflowSize = 0
      // Stats
      {
        val engine = new Executor(DRedReteBackendFactory.INSTANCE).instantiate(compiled, dataModel)
        engine.feed.processEditScript(s.loadEdits)

        val rels = engine.readAll()
        val stats = ("total" -> IndexedSeq(rels.map(_.size).sum.toLong)) +: engine.readAll().map { r =>
          r.name -> IndexedSeq(r.size.toLong)
        }
        val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))
        cflowSize = cflowRel.size

        FileUtil.writeFile(s"$resultPath/interval/Interval_DL_${cflowSize}_stats.csv", csvToString(toCSV(stats)))
      }

      assert(cflowSize != 0)

      collectGarbage()

      // Warmup
      for (k <- Range.inclusive(1, warmups)) {
        val engine = new Executor(DRedReteBackendFactory.INSTANCE).instantiate(compiled, dataModel)
        engine.feed.processEditScript(s.loadEdits)
        engine.measure(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))

        collectGarbage()
      }

      // Measurement run
      cflowSize.toString -> (for (j <- Range.inclusive(1, runs)) yield {
        collectGarbage()

        val engine = new Executor(DRedReteBackendFactory.INSTANCE).instantiate(compiled, dataModel)

        val startLoad = System.nanoTime()
        engine.feed.processEditScript(s.loadEdits)
        val endLoad = System.nanoTime()
        val loadTime = (endLoad - startLoad)

        val propTime = engine.measure(Relation3("main", Seq("exit", "x", "x_iv"), Seq()))
        val cflowRel = engine.read(Relation2("cflow", Seq("from", "to"), Seq()))

        println(s"${cflowRel.size} cflow entries")
        println(s"Load time ${loadTime / 1000000}ms")
        println(s"Propagation time ${propTime / 1000000}ms")
        println()

        propTime
      })
    }

    FileUtil.writeFile(s"$resultPath/interval/Interval_DL.csv", csvToString(toCSV(measurements)))
  }

  @main def dlCheck1 = {
    run(Benchmark.example1)
  }

  @main def dlCheck2 = {
    run(Benchmark.example2)
  }

  @main def dlCheck3 = {
    run(Benchmark.example3)
  }

  @main def dlCheck4 = {
    run(Benchmark.example4)
  }


  @main def dlCheck5 = {
    run(Benchmark.example5)
  }

  @main def dlCheck6 = {
    run(Benchmark.example6)
  }

  @main def dlCheck7 = {
    run(Benchmark.example7)
  }

  @main def dlCheck8 = {
    run(Benchmark.example8)
  }


  @main def dlCheck9 = {
    run(Benchmark.example9)
  }

  @main def dlCheck10 = {
    run(Benchmark.example10)
  }

  @main def dlCheck11 = {
    run(Benchmark.example11)
  }

  @main def dlCheck12 = {
    run(Benchmark.example12)
  }


  @main def dlCheck13 = {
    run(Benchmark.example13)
  }

  @main def dlCheck14 = {
    run(Benchmark.example14)
  }

  @main def dlCheck15 = {
    run(Benchmark.example15)
  }

  @main def dlCheck16 = {
    run(Benchmark.example16)
  }


  @main def dlCheck17 = {
    run(Benchmark.example17)
  }

  @main def dlCheck18 = {
    run(Benchmark.example18)
  }

  @main def dlCheck19 = {
    run(Benchmark.example19)
  }

  @main def dlCheck20 = {
    run(Benchmark.example20)
  }

  @main def dlCheck21 = {
    run(Benchmark.example21)
  }

  @main def dlCheck22 = {
    run(Benchmark.example22)
  }

  @main def dlCheck23 = {
    run(Benchmark.example23)
  }

  @main def dlCheck24 = {
    run(Benchmark.example24)
  }

  @main def dlCheck25 = {
    run(Benchmark.example25)
  }
