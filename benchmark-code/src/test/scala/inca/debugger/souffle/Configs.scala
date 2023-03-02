package inca.debugger.souffle

import inca.compiler.source.SourceFile
import inca.debugger.ScalaValue
import inca.debugger.ValueTable
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.lowering.SouffleToNamedRelations
import inca.frontend.souffle.parser.Parser
import inca.measurements.util.Config
import inca.runtime.db.DatabaseInput
import java.io.File

object Configs {
  val warmup: Int = 0
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

  trait BaseConfig extends Config {
    val doopProg: DoopProgram
    val entry: String
    val args: ValueTable
    lazy val compiled: CompiledSouffleModule = readSouffleProgram(varPointsToPath)
    lazy val input: DatabaseInput = readSouffleInput(compiled, doopProg.fullPath)
  }

  case class BottomUpVTopDownConfig(
      doopProg: DoopProgram,
      entry: String,
      args: ValueTable,
      warmup: Int,
      runs: Int)
      extends BaseConfig {
    def name: String = s"VarPointsTo_${doopProg.path}_${entry}_${args.columns.mkString(";")}"
  }

  case class StepIntoVStepOverConfig(
      doopProg: DoopProgram,
      entry: String,
      args: ValueTable,
      warmup: Int,
      runs: Int)
      extends BaseConfig {
    def name: String = s"VarPointsTo_${doopProg.path}_${entry}_${args.columns.mkString(";")}"
  }

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
      warmup: Int,
      runs: Int
    ): BottomUpVTopDownConfig = {
    BottomUpVTopDownConfig(doopProg, entry, args, warmup, runs)
  }

  def baseConfig(doopProg: DoopProgram, entry: String, t: ValueTable): BaseConfig =
    souffleVarPointsToConfig(doopProg, entry, t, warmup, runs)

  def methodLookupConfig(doopProg: DoopProgram, t: ValueTable): BaseConfig =
    baseConfig(doopProg, "basic_MethodLookup", t)

  def subtypeOf(doopProg: DoopProgram, t: ValueTable): BaseConfig =
    baseConfig(doopProg, "basic_SubtypeOf", t)

  def varPointsToConfig(doopProg: DoopProgram, t: ValueTable): BaseConfig =
    baseConfig(doopProg, "basic_MethodLookup", t)

  // step-into produces result in reasonable amount of time
  // find all methods with the name accept and signature java.lang.Object(visitor.GJVisitor,java.lang.Object)
  // will produce 50 tuples complete MethodLookup will have 86005 tuples
  def scenario1v1(doopProg: DoopProgram): BaseConfig =
    methodLookupConfig(
      doopProg,
      ValueTable(
        Seq("simplename", "descriptor"),
        Seq(
          Seq(
            ScalaValue("accept"),
            ScalaValue("java.lang.Object(visitor.GJVisitor,java.lang.Object)")))))
  // step-into produces result in reasonable amount of time
  // find all methods with the name accept
  // will produce 239 tuples complete MethodLookup will have 86005 tuples
  def scenario1v2(doopProg: DoopProgram): BaseConfig =
    methodLookupConfig(doopProg, ValueTable(Seq("simplename"), Seq(Seq(ScalaValue("accept")))))
  // ground tuple as entry
  def scenario1v3(doopProg: DoopProgram): BaseConfig = methodLookupConfig(
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
    )
  )
  def scenario1v4(doopProg: DoopProgram): BaseConfig =
    methodLookupConfig(doopProg, ValueTable.unit())

  def scenario2v1(doopProg: DoopProgram): BaseConfig = subtypeOf(
    doopProg,
    ValueTable(
      Seq("subtype"),
      Seq(
        Seq(ScalaValue("sun.reflect.generics.tree.BooleanSignature")),
        Seq(ScalaValue("sun.reflect.generics.tree.ClassTypeSignature"))))
  )

  def scenario3v1(doopProg: DoopProgram): BaseConfig =
    varPointsToConfig(
      doopProg,
      ValueTable(
        Seq("var", "heap"),
        Seq(
          Seq(
            ScalaValue(
              "<typechecking.ClassSymbol: int putMethod(java.lang.String,typechecking.MethodSymbol)>/$r1"),
            ScalaValue("<<HASH:1808431609>>")))
      )
    )
  def scenario3v2(doopProg: DoopProgram): BaseConfig = varPointsToConfig(
    doopProg,
    ValueTable(
      Seq("var"),
      Seq(Seq(ScalaValue(
        "<typechecking.ClassSymbol: int putMethod(java.lang.String,typechecking.MethodSymbol)>/$r1")))))
}
