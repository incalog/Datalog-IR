package inca.casestudy.fundeps

import inca.ir.{BaseIR, Body, Call, CompiledUnit, Eq, ExtensionalCall, ExtensionalRelation, FunctionalDependencyHint, MainHint, Module, Name, Param, Relation, Var, execution, string2name, term2Arg, termList2ArgList}
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{Add, IntNum, Sub, TInt}
import inca.ir.extension.arithmetic.IR as arithIR
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.{arithmetic, block, bool, demand, disjunction, string, tuple}
import inca.ir.optimize.{AliasElimination, DisjointRuleAnalysis}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions

// TODO: Viatra seems to fully ignore this
object Experiment:

  val inputRel = Relation("input", Seq(
    Param("n", TInt),
  ), Seq(
    Body(Seq(
      Call("input", Seq(Var("n$0"))),
      Eq(Var("n$0"), IntNum(0), true),
      Eq(Var("n"), Sub(Var("n$0"), IntNum(1)))
    )),
    Body(Seq(
      ExtensionalCall("input$main", Seq(Var("n")))
    ))
  ))

  val fibRel = Relation("fib", Seq(
    Param("n", TInt),
    Param("d", TInt),
    Param("r", TInt)
  ), Seq(
    Body(Seq(
      Call("input", Seq(Var("n"))),
      ExtensionalCall("input$dummy", Seq(Var("d"))),
      Eq(Var("n"), IntNum(0)),
      Eq(Var("r"), IntNum(0))
    )),
    Body(Seq(
      Call("input", Seq(Var("n"))),
      ExtensionalCall("input$dummy", Seq(Var("d"))),
      Eq(Var("n"), IntNum(1)),
      Eq(Var("r"), IntNum(1))
    )),
    Body(Seq(
      Call("input", Seq(Var("n"))),
      ExtensionalCall("input$dummy", Seq(Var("d"))),
      Eq(Var("n"), IntNum(0), true),
      Eq(Var("n"), IntNum(1), true),
      Call("fib", Seq(Sub(Var("n"), IntNum(1)), Var("d"), Var("r$0"))),
      Call("fib", Seq(Sub(Var("n"), IntNum(2)), Var("d"), Var("r$1"))),
      Eq(Var("r"), Add(Var("r$0"), Var("r$1")))
    )),
  )).addHint(FunctionalDependencyHint(Seq("n"), Seq("r")))

  val mainRel = Relation("main", Seq(
    Param("n", TInt),
    Param("d", TInt),
    Param("r", TInt)
  ), Seq(
    Body(Seq(
      ExtensionalCall("input$main", Seq(Var("n"))),
      ExtensionalCall("input$dummy", Seq(Var("d"))),
      Call("fib", Seq(Var("n"), Var("d"), Var("r"))),
    )),
  )).addHint(MainHint)
    .addHint(FunctionalDependencyHint(Seq("n"), Seq("r")))

  val inputMain = ExtensionalRelation("input$main", Seq(Param("n", TInt)))

  val inputDummy = ExtensionalRelation("input$dummy", Seq(Param("d", TInt)))

  val mainModule = Module("Experiment", BaseIR.language + arithIR, Seq(
    inputDummy, inputMain, inputRel, fibRel, mainRel
  ))

  def createCompiled(mod: Module): CompiledUnit = new CompiledUnit:
    override def name: Name = mod.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override val irModules: Seq[Module] = Seq(mod)
    override val otherUnits: Seq[CompiledUnit] = Seq()
    override val isClosedWorld = true

    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logModule = true
      opt.irLogging.logLowerings = true
      opt.irLogging.logTypeInformation = false
      opt.irLogging.logStatsAfterOptimizations = false
      opt
    }

    setPipeline(List(
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {},
      () => new AliasElimination {}
    ))

    /*setOptimizationPipeline(List(
      () => new DisjointRuleAnalysis {}
    ))*/


  private def runModInEngine(executor: IRExecutor, mod: Module): Unit =
    val compiled = createCompiled(mod)

    val data = Seq(
      execution.Relation1("input$dummy", Seq("n"), 0.until(50000).map(Seq(_))),
      execution.Relation1("input$main", Seq("n"), Seq(Seq(10)))
    )

    // Measure
    val measurements = for (i <- 0.until(20)) yield {
      val engine = executor.instantiate(compiled)
      data.foreach(engine.insert)
      engine.measure(UnitRelation("main"))
    }

    measurements.foreach(println)

    //engine.readAll().foreach(r => println(r.asTable))

  @main def runProg() =
    inca.viatra.backend.Executor.initializeLogging()
    inca.viatra.backend.Executor.enableDebugLogging()

    val viatraRes = runModInEngine(inca.viatra.backend.Executor(), mainModule)
    //println(viatraRes.asTable)