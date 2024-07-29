package inca.frontend.functional.executor.varpointsto

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.Relation1
import inca.ir.extension.*
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{CompiledProgram, CompiledUnit, Module, Name, optimize}
import inca.souffle.frontend.compile.{CompiledSouffleProgram, CompiledSouffleUnit, GenerateIR as SouffleGenerateIR}
import inca.souffle.syntax.Parser
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.string2name

import scala.io.Source

class FunctionalViatraExecutorTest extends AnyFunSuite:
  val souffle_pipeline: List[() => BaseIRVisitor] = List(
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new module.Lowering {},
    // optimize
    () => new optimize.AliasElimination {}
  ) // arith + string + data

  test("VarPointsTo") {
    // Create Souffle Module
    val baseDir = "functional/escapeanalysis"
    val source = Source.fromResource(s"$baseDir/MicroDoop.dl")
    val compiledSouffleProgram = CompiledSouffleProgram.fromSource("MicroDoop", source)
    compiledSouffleProgram.setPipeline(souffle_pipeline)

    val edbFacts = compiledSouffleProgram.loadEdbInputs(s"$baseDir/minijavac")
    val outputRels = compiledSouffleProgram.outputRelations

    // Create FunIncA module
    val code = FileUtil.readFileFromResource(s"$baseDir/EscapeAnalysis.finca")
    val options = FunctionalCompilerOptions.default

    val exec: FunctionalExecutor = new FunctionalExecutor(inca.souffle.backend.Executor())
    val compiled = exec.compileFunction(code, options, compiledSouffleProgram.compiledUnits)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    //compiled.setPostProcessingPipeline(CompiledFunctionalUnit.viatraPostProcessingPipeline)

    edbFacts.foreach { fact =>
      fact.name = "md$" + fact.name
    }

    //println(compiled.lowered)

    val loaded = exec.loadFunction(compiled)
    edbFacts.foreach(loaded.engine.insert)
    var res = loaded.execute("main", Seq("main")) // 2, "syntaxtree.TrueLiteral"
    //loaded.engine.readAll().foreach(r => println(s"${r.name} :: ${r.size}"))
    // This fails if the object can not escape
    val setAdt = res.entries.head
    //res = loaded.engine.read(Relation1("Set$$TString_TString_TString_TString_TString_TString_TInt$$enum", Seq("x"), Seq(Seq(setAdt))))
    res = loaded.engine.read(Relation1("Set$TBoolean$enum", Seq("x"), Seq(Seq(setAdt))))
    println(res.asTable)
  }
