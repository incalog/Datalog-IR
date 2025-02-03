package inca.casestudy.boolean

import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.ir.*
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.bool.{BoolAnd, BoolNot, BoolTrue, TBoolean}
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

object BooleanExpression:


  val inputMain = ExtensionalRelation(
    "input$main",
    Seq(
      Param("x", TBoolean),
      Param("y", TBoolean)
    )
  )

  def main =
    val x = Var("x")
    val y = Var("y")
    val r = Var("r")
    val input = ExtensionalCall("input$main", Seq(x, y))
    Relation("main",
      Seq(
        Param("x", TBoolean),
        Param("y", TBoolean),
        Param("r", TInt),
      ),
      Seq(
        Body(input +: Seq(Eq(BoolAnd(x, BoolAnd(x, y)), BoolTrue), Eq(r, IntNum(0)))),
        Body(input +: Seq(Eq(BoolAnd(x, BoolNot(BoolAnd(x, y))), BoolTrue), Eq(r, IntNum(1)))),
        Body(input +: Seq(Eq(BoolAnd(BoolAnd(BoolNot(x), y), BoolAnd(x, y)), BoolTrue), Eq(r, IntNum(2)))),
        Body(input +: Seq(Eq(BoolAnd(BoolAnd(BoolNot(x), y), BoolNot(BoolAnd(x, y))), BoolTrue), Eq(r, IntNum(3)))),
        Body(input +: Seq(Eq(BoolAnd(BoolNot(x), BoolNot(y)), BoolTrue), Eq(r, IntNum(4)))),
      )
    )


  def createMod() = Module("BooleanConditional", BaseIR.language + arithmetic.IR + bool.IR,
    Seq(
      inputMain,
      main
    )
  )


  def createCompiled(mod: Module, optimizeSets: Boolean): CompiledUnit = new CompiledUnit:
    override def name: Name = mod.name

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

    override def irModules: Seq[Module] = Seq(mod)

    override def otherUnits: Seq[CompiledUnit] = Seq()

    override val isClosedWorld = true

    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logLowerings = true
      opt.irLogging.logTypeInformation = false
      opt.irLogging.logStatsAfterOptimizations = false
      opt
    }

    val optim = if optimizeSets then
      List(() => new bool.optimize.DnfOptimizer {})
    else
      List()

    setPipeline(optim ++ List(
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
    engine.insert(execution.Relation2("input$main", Seq("x", "y"), Seq(Seq(1, 1))))
    engine.read(UnitRelation("main"))

  @main def runBooleanExpression() =
    val viatraRes = runModInEngine(inca.viatra.backend.Executor(), createMod())
    println(viatraRes.asTable)

  