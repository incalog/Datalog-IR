package inca.casestudy.termination

import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.ir.*
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.bool.{BoolAnd, BoolNot, BoolTrue, TBoolean}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.demand.*
import inca.ir.extension.set
import inca.ir.extension.set.SyntacticOptimizer
import inca.ir.optimize.AliasElimination
import inca.ir.util.SourceLocation
import inca.util.{CSVUtil, FileUtil}
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend.Executor
import org.jfree.chart.{LegendItem, LegendItemCollection, LegendItemSource}
import org.jfree.chart.annotations.XYTitleAnnotation
import org.jfree.chart.block.{BlockBorder, ColumnArrangement, FlowArrangement}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.title.LegendTitle
import org.jfree.chart.ui.{RectangleAnchor, RectangleEdge, VerticalAlignment}

import java.awt.Color

// plotting
import breeze.linalg._
import breeze.plot._

import scala.language.implicitConversions

object Termination:

  val TList = TData("TList")

  def data = Seq(
    DataDefinition("TList"),
    CaseDefinition("TNil", Seq(), TList),
    CaseDefinition("TCons", Seq(TInt, TList), TList),
  )

  def main = Relation("main",
    Seq(
      Param("x", TList),
    ),
    Seq(
      Body(Seq(
        Call("buildList", Seq(Var("x")))
      ))
    )
  )

  def recursive = Relation("buildList",
    Seq(
      Param("y", TList),
    ),
    Seq(
      Body(Seq(
        Eq(Var("y"), Construct("TNil", Seq()))
      )),
      Body(Seq(
        Call("buildList", Seq(Var("z"))),
        Deconstruct(Var("z"), "TCons", Seq(WildcardArg(), WildcardArg()), true),
        Eq(Var("y"), Construct("TCons", Seq(IntNum(0), Var("z"))))
      ))
    )
  )


  def createMod() = Module("Termination", BaseIR.language + arithmetic.IR + bool.IR,
    data ++ Seq(
      main,
      recursive
    )
  )


  def createCompiled(mod: Module, optimizeBools: Boolean): CompiledUnit = new CompiledUnit:
    override def name: Name = mod.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override val irModules: Seq[Module] = Seq(mod)
    override val otherUnits: Seq[CompiledUnit] = Seq()
    override val isClosedWorld = true

    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logLowerings = true
      opt.irLogging.logTypeInformation = false
      opt.irLogging.logStatsAfterOptimizations = false
      opt
    }

    setPipeline(List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new demand.Lowering {},
      () => new tuple.Lowering {},
      () => new AliasElimination {}
    ))


  private def runModInEngine(executor: IRExecutor, mod: Module): execution.Relation =
    val compiled = createCompiled(mod, true)
    val engine = executor.instantiate(compiled)
    engine.read(UnitRelation("main"))

  @main def runNonTerminating(): Unit =
    val viatraRes = runModInEngine(inca.viatra.backend.Executor(), createMod())
    println(viatraRes.asTable)

  