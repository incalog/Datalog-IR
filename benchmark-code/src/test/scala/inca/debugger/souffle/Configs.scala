package inca.debugger.souffle

import inca.backend.ir.Datalog
import inca.compiler.source.SourceFile
import inca.debugger.AccumulatingDebuggerState
import inca.debugger.Atom
import inca.debugger.AvoidNonProducingIterationDebuggerState
import inca.debugger.DebuggerState
import inca.debugger.DelayingDebuggerState
import inca.debugger.Predicate
import inca.debugger.Query
import inca.debugger.QueryResult
import inca.debugger.ResettingDebuggerState
import inca.debugger.Rule
import inca.debugger.ScalaValue
import inca.debugger.Subquery
import inca.debugger.ValueTable
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.lowering.SouffleToNamedRelations
import inca.frontend.souffle.parser.Parser
import inca.measurements.util.Config
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import java.io.File

object Configs {
  val warmup: Int = 5
  val runs: Int = 1
  val varPointsToPath = "souffle-frontend/benchmark/self-contained.dl"
  val factsPath = "souffle-frontend/benchmark/facts/"

  sealed trait DoopProgram {
    def fullPath: String = s"$factsPath/$path"
    def path: String
  }
  object DoopProgram {
    val programs: Seq[DoopProgram] = Seq(MiniJavac, Antlr, Ant, JEdit, Emma, Pmd)
    case object MiniJavac extends DoopProgram {
      val path: String = "minijavac"
    }
    case object MiniJavacSlim extends DoopProgram {
      val path: String = "minijavac-slim"
    }
    case object Antlr extends DoopProgram {
      val path: String = "antlr"
    }
    case object Ant extends DoopProgram {
      val path: String = "ant"
    }
    case object JEdit extends DoopProgram {
      val path: String = "jedit"
    }
    case object Emma extends DoopProgram {
      val path: String = "emma"
    }
    case object Pmd extends DoopProgram {
      val path: String = "pmd"
    }
  }

  sealed trait DebuggingSemantics {
    def name: String
    def debuggingState: DatalogRuntime => DebuggerState
  }
  object DebuggingSemantics {
    case object PureIntoSemantics extends DebuggingSemantics {
      override def name: String = "PureInto"
      override def debuggingState: DatalogRuntime => DebuggerState = (rt: DatalogRuntime) =>
        new DelayingDebuggerState(rt)
    }
    case object PureIntoOptSemantics extends DebuggingSemantics {
      override def name: String = "PureIntoOpt"
      override def debuggingState: DatalogRuntime => DebuggerState = (rt: DatalogRuntime) =>
        new DelayingDebuggerState(rt) with AvoidNonProducingIterationDebuggerState
    }
    case object HybridSemantics extends DebuggingSemantics {
      override def name: String = "HybridSemantics"
      override def debuggingState: DatalogRuntime => DebuggerState = (rt: DatalogRuntime) =>
        new DelayingDebuggerState(rt)
    }
    case object HybridOptSemantics extends DebuggingSemantics {
      override def name: String = "HybridSemanticsOpt"
      override def debuggingState: DatalogRuntime => DebuggerState = (rt: DatalogRuntime) =>
        new DelayingDebuggerState(rt) with AvoidNonProducingIterationDebuggerState
    }
  }

  trait BaseConfig extends Config {
    val doopProg: DoopProgram
    val entry: String
    val args: ValueTable
    val semantics: DebuggingSemantics
    lazy val compiled: CompiledSouffleModule = readSouffleProgram(varPointsToPath)
    lazy val input: DatabaseInput = readSouffleInput(compiled, doopProg.fullPath)
  }

  case class SimpleConfig(
      doopProg: DoopProgram,
      entry: String,
      args: ValueTable,
      semantics: DebuggingSemantics,
      warmup: Int,
      runs: Int)
      extends BaseConfig {
    def name: String =
      s"VarPointsTo_${doopProg.path}_${entry}_${args.columns.mkString(";")}_${semantics.name}"
  }

//  case class BottomUpVTopDownConfig(
//      doopProg: DoopProgram,
//      entry: String,
//      args: ValueTable,
//      warmup: Int,
//      runs: Int)
//      extends BaseConfig {
//    def name: String = s"VarPointsTo_${doopProg.path}_${entry}_${args.columns.mkString(";")}"
//  }
//
//  case class StepIntoVStepOverConfig(
//      doopProg: DoopProgram,
//      entry: String,
//      args: ValueTable,
//      warmup: Int,
//      runs: Int)
//      extends BaseConfig {
//    def name: String = s"VarPointsTo_${doopProg.path}_${entry}_${args.columns.mkString(";")}"
//  }

  def readSouffleProgram(path: String): CompiledSouffleModule = {
    val file = new File(path)
    val analysis = Parser.parse(SourceFile(file.toPath))
    val compiler = new SouffleToDatalogIR(false)
    compiler.compile("SouffleModule", analysis)
  }

  def readSouffleInput(compiled: CompiledSouffleModule, path: String): DatabaseInput = {
    val inputCompiler = new SouffleToNamedRelations(path)
    inputCompiler.compile(compiled.inputs.values.map(x => x._2 -> x._1).toMap)
  }

  def souffleVarPointsToConfig(
      doopProg: DoopProgram,
      entry: String,
      args: ValueTable,
      semantics: DebuggingSemantics,
      warmup: Int,
      runs: Int
    ): SimpleConfig = {
    SimpleConfig(doopProg, entry, args, semantics, warmup, runs)
  }

  def baseConfig(
      doopProg: DoopProgram,
      entry: String,
      t: ValueTable,
      semantics: DebuggingSemantics
    ): BaseConfig =
    souffleVarPointsToConfig(doopProg, entry, t, semantics, warmup, runs)

  def methodLookupConfig(
      doopProg: DoopProgram,
      t: ValueTable,
      semantics: DebuggingSemantics
    ): BaseConfig =
    baseConfig(doopProg, "basic_MethodLookup", t, semantics)

  def subtypeOf(doopProg: DoopProgram, t: ValueTable, semantics: DebuggingSemantics): BaseConfig =
    baseConfig(doopProg, "basic_SubtypeOf", t, semantics)

  def varPointsToConfig(
      doopProg: DoopProgram,
      t: ValueTable,
      semantics: DebuggingSemantics
    ): BaseConfig =
    baseConfig(doopProg, "VarPointsTo", t, semantics)

  // step-into produces result in reasonable amount of time
  // find all methods with the name accept and signature java.lang.Object(visitor.GJVisitor,java.lang.Object)
  // will produce 50 tuples complete MethodLookup will have 86005 tuples
  def scenario1v1(doopProg: DoopProgram, semantics: DebuggingSemantics): BaseConfig =
    methodLookupConfig(
      doopProg,
      ValueTable(
        Seq("simplename", "descriptor"),
        Seq(
          Seq(
            ScalaValue("accept"),
            ScalaValue("java.lang.Object(visitor.GJVisitor,java.lang.Object)")))),
      semantics
    )
  // step-into produces result in reasonable amount of time
  // find all methods with the name accept
  // will produce 239 tuples complete MethodLookup will have 86005 tuples
  def scenario1v2(doopProg: DoopProgram, semantics: DebuggingSemantics): BaseConfig =
    methodLookupConfig(
      doopProg,
      ValueTable(Seq("simplename"), Seq(Seq(ScalaValue("accept")))),
      semantics)
  // ground tuple as entry
  def scenario1v3(doopProg: DoopProgram, semantics: DebuggingSemantics): BaseConfig =
    methodLookupConfig(
      doopProg,
      ValueTable(
        Seq("simplename", "descriptor", "type", "method"),
        Seq(
          Seq(
            ScalaValue("accept"),
            ScalaValue("boolean(java.lang.Object"),
            ScalaValue("java.nio.file.File#1"),
            ScalaValue("<sun.misc.JarFilter: boolean accept(java.io.File,java.lang.String)>")
          ))
      ),
      semantics
    )
  def scenario1v4(doopProg: DoopProgram, semantics: DebuggingSemantics): BaseConfig =
    methodLookupConfig(doopProg, ValueTable.unit(), semantics)

  def scenario2v1(doopProg: DoopProgram, semantics: DebuggingSemantics): BaseConfig = subtypeOf(
    doopProg,
    ValueTable(
      Seq("subtype"),
      Seq(
        Seq(ScalaValue("sun.reflect.generics.tree.BooleanSignature")),
        Seq(ScalaValue("sun.reflect.generics.tree.ClassTypeSignature")))),
    semantics
  )

  def scenario3v1(doopProg: DoopProgram, semantics: DebuggingSemantics): BaseConfig =
    varPointsToConfig(
      doopProg,
      ValueTable(
        Seq("var", "heap"),
        Seq(
          Seq(
            ScalaValue(
              "<typechecking.ClassSymbol: int putMethod(java.lang.String,typechecking.MethodSymbol)>/$r1"),
            ScalaValue("<<HASH:1808431609>>")))
      ),
      semantics
    )
  def scenario3v2(doopProg: DoopProgram, semantics: DebuggingSemantics): BaseConfig =
    varPointsToConfig(
      doopProg,
      ValueTable(
        Seq("var"),
        Seq(Seq(ScalaValue(
          "<typechecking.ClassSymbol: int putMethod(java.lang.String,typechecking.MethodSymbol)>/$r1")))),
      semantics
    )

  def constructIntoPredsOracle(intoPreds: Set[Predicate]): Oracle = {
    val f: Query => (Boolean, Predicate) = {
      case Subquery(_, _, _, _, Rule(_, _, Atom(Datalog.Call(callee, _, _, _)) +: _) +: _) =>
        (intoPreds.contains(callee), callee)
      case Subquery(pred, _, _, _, _) => (true, pred)
      case QueryResult(pred, _, _) => (true, pred)
    }
    Oracle(f, s"Into${intoPreds.mkString("And")}")
  }

  def constructOverPredsOracle(overPreds: Set[Predicate]): Oracle = {
    val f: Query => (Boolean, Predicate) = {
      case Subquery(_, _, _, _, Rule(_, _, Atom(Datalog.Call(callee, _, _, _)) +: _) +: _) =>
        (!overPreds.contains(callee), callee)
      case Subquery(pred, _, _, _, _) => (true, pred)
      case QueryResult(pred, _, _) => (true, pred)
    }
    Oracle(f, s"Over${overPreds.mkString("And")}")
  }

  case class Oracle(f: Query => (Boolean, Predicate), name: String) {
    def shouldStepInto: Query => (Boolean, Predicate) = f
  }

  // into pred scenarios
  val scenario3Orcale1: Oracle = constructIntoPredsOracle(Set("VarPointsTo"))
  val scenario3Orcale2: Oracle = constructIntoPredsOracle(Set("VarPointsTo", "StaticFieldPointsTo"))
  val scenario3Orcale3: Oracle = constructIntoPredsOracle(
    Set("VarPointsTo", "InstanceFieldPointsTo"))
  val scenario3Orcale4: Oracle = constructIntoPredsOracle(Set("VarPointsTo", "Reachable"))

  // over pred scenarios
}
