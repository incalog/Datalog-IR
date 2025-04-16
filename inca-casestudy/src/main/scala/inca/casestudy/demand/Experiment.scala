package inca.casestudy.demand

import inca.ir.{BaseIR, Body, Call, CompiledUnit, Eq, ExtensionalCall, ExtensionalRelation, FunctionalDependencyHint, Module, Name, Param, Relation, Var, execution, string2name, term2Arg, termList2ArgList}
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{Add, IntNum, Sub, TInt, Mul}
import inca.ir.extension.arithmetic.IR as arithIR
import inca.ir.extension.demand.IR as demandIR
import inca.ir.extension.demand.TDemand
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.{arithmetic, block, bool, demand, disjunction, string, tuple}
import inca.ir.hints.MainHint
import inca.ir.optimize.{AliasElimination, DisjointRuleAnalysis}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions

object Experiment:

  val factRel = Relation("fact", Seq(
    Param("n", TDemand(TInt)),
    Param("r", TInt)
  ), Seq(
    Body(Seq(
      Eq(Var("n"), IntNum(1)),
      Eq(Var("r"), IntNum(1))
    )),
    Body(Seq(
      Eq(Var("n"), IntNum(1), true),
      Call("fact", Seq(Sub(Var("n"), IntNum(1)), Var("res"))),
      Eq(Var("r"), Mul(Var("n"), Var("res")))
    )),
  ))

  val mainRel = Relation("main", Seq(
    Param("n", TInt),
    Param("r", TInt)
  ), Seq(
    Body(Seq(
      ExtensionalCall("input$main", Seq(Var("n"))),
      Call("fact", Seq(Var("n"), Var("r"))),
    )),
  )).addHint(MainHint)

  val inputMain = ExtensionalRelation("input$main", Seq(Param("n", TInt)))

  val mainModule = Module("Experiment", BaseIR.language + arithIR + demandIR, Seq(
    inputMain, factRel, mainRel
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
    val engine = executor.instantiate(compiled)
    engine.insert(execution.Relation1("input$main", Seq("n"), Seq(Seq(2))))
    val res = engine.read(UnitRelation("main"))
    engine.readAll().foreach(r => println(r.asTable))

  @main def runProg(): Unit =
    runModInEngine(inca.viatra.backend.Executor(), mainModule)
