package inca.casestudy.termination

import inca.casestudy.util.Util.{collectGarbage, toCSV}
import inca.ir.*
import inca.ir.execution.ThreadCount.Fixed
import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.*
import inca.ir.extension.arithmetic.IR as arithIR
import inca.ir.extension.bool.{BoolAnd, BoolNot, BoolTrue, TBoolean}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.demand.*
import inca.ir.extension.set
import inca.ir.extension.set.SyntacticOptimizer
import inca.ir.extension.string.TString
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


  private def runModInEngine(executor: IRExecutor, mod: Module, edb: Seq[execution.Relation] = Seq()): Seq[execution.Relation] =
    val compiled = createCompiled(mod, true)
    val engine = executor.instantiate(compiled)
    edb.foreach(engine.insert)
    engine.readAll()

  @main def runNonTerminatingADT(): Unit =
    val TList = TData("TList")

    val data = Seq(
      DataDefinition("TList"),
      CaseDefinition("TNil", Seq(), TList),
      CaseDefinition("TCons", Seq(TInt, TList), TList),
    )

    val main = Relation("main",
      Seq(
        Param("x", TList),
      ),
      Seq(
        Body(Seq(
          Call("buildList", Seq(Var("x")))
        ))
      )
    )

    val recursive = Relation("buildList",
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

    val mod = Module("Termination", BaseIR.language + arithmetic.IR + bool.IR,
      data ++ Seq(
        main,
        recursive
      )
    )

    val res = runModInEngine(inca.viatra.backend.Executor(), mod)
    res.foreach(r => println(r.asTable))


  @main def runNonTerminatingInt(): Unit =
    val mod = Module("MethodLookup", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("DirectSuperclass", Seq(
        Param("type", TString),
        Param("supertype", TString)
      )),

      ExtensionalRelation("MethodImplemented", Seq(
        Param("type", TString),
        Param("method", TString)
      )),

      /*Relation("MethodImplemented", Seq(
        Param("type", TString),
        Param("method", TString)
      ), Seq(
        Body(Seq(ExtensionalCall("_MethodImplemented", Seq(Var("type"), Var("method")))))
      )),*/

      Relation("_MethodLookup_WithLen", Seq(
        Param("type", TString),
        Param("method", TString),
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("method"))),
          Eq(Var("n"), IntNum(0))
        )),
        Body(Seq(
          ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("supertype"))),
          Call("_MethodLookup_WithLen", Seq(Var("supertype"), Var("method"), Var("n0"))),
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("_")), true),
          Eq(Var("n"), Add(Var("n0"), IntNum(1)))
        ))
      ))
    ))

    val edb = Seq(
      execution.Relation2(
        "DirectSuperclass", Seq("type", "supertype"), Seq(
          Seq("C", "A"),
          Seq("B", "A"),
          Seq("C", "B"),
          Seq("B", "C"), // This fact closes the cycle and produces and endless-loop
        )
      ),
      execution.Relation2(
        "MethodImplemented", Seq("type", "method"), Seq(
          Seq("A", "test"),
        )
      )
    )

    val res = runModInEngine(inca.souffle.backend.Executor(), mod, edb)
    res.foreach(r => println(r.asTable))

  @main def runDefensiveTerminatingInt(): Unit =
    val mod = Module("MethodLookup", BaseIR.language + arithIR, Seq(
      ExtensionalRelation("DirectSuperclass", Seq(
        Param("type", TString),
        Param("supertype", TString)
      )),

      ExtensionalRelation("MethodImplemented", Seq(
        Param("type", TString),
        Param("method", TString)
      )),

      /*Relation("MethodImplemented", Seq(
        Param("type", TString),
        Param("method", TString)
      ), Seq(
        Body(Seq(ExtensionalCall("_MethodImplemented", Seq(Var("type"), Var("method")))))
      )),*/

      Relation("TransitiveSuperclasses",
        Seq(
          Param("type", TString),
          Param("supertype", TString)
        ),
        Seq(
          Body(Seq(
            ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("supertype"))),
          )),
          Body(Seq(
            ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("stype"))),
            Call("TransitiveSuperclasses", Seq(Var("stype"), Var("supertype"))),
          ))
        )
      ),

      Relation("_MethodLookup_WithLen", Seq(
        Param("type", TString),
        Param("method", TString),
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("method"))),
          Eq(Var("n"), IntNum(0))
        )),
        Body(Seq(
          ExtensionalCall("DirectSuperclass", Seq(Var("type"), Var("supertype"))),
          Call("_MethodLookup_WithLen", Seq(Var("supertype"), Var("method"), Var("n0"))),
          Aggregate("TransitiveSuperclasses", Seq(Var("type").arg, AggregateColumnArg(Var("c"))), ArithmeticAggregationOperator.Count),
          LE(Var("c"), Var("n0")),
          ExtensionalCall("MethodImplemented", Seq(Var("type"), Var("_")), true),
          Eq(Var("n"), Add(Var("n0"), IntNum(1)))
        ))
      ))
    ))

    val edb = Seq(
      execution.Relation2(
        "DirectSuperclass", Seq("type", "supertype"), Seq(
          Seq("C", "A"),
          Seq("B", "A"),
          Seq("C", "B"),
          Seq("B", "C"), // This fact closes the cycle and produces and endless-loop
        )
      ),
      execution.Relation2(
        "MethodImplemented", Seq("type", "method"), Seq(
          Seq("A", "test"),
        )
      )
    )

    val res = runModInEngine(inca.souffle.backend.Executor(), mod, edb)
    res.foreach(r => println(r.asTable))