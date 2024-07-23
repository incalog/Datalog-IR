package inca.frontend.functional.executor.interop

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.visitors.BaseIRVisitor
import inca.ir.extension.*
import inca.ir.{CompiledProgram, CompiledUnit, Module, Name, optimize}
import inca.ir.util.SourceLocation
import inca.souffle.frontend.compile.GenerateIR as SouffleGenerateIR
import inca.souffle.syntax.Parser
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

class FunctionalViatraExecutorTest extends AnyFunSuite:
  case class CompiledSouffleModule(irModules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean, name: Name) extends CompiledUnit:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logLowerings = false
      opt.irLogging.logOptimizations = false
      opt

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

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

  test("Minimal interoperability") {
    val souffleDep = FileUtil.readFileFromResource("functional/interop/SouffleHelper.dl")
    val prog = Parser.parseSouffle(souffleDep)
    val genIR = SouffleGenerateIR()
    val generateMods = genIR.compileProgram(prog, "SouffleHelper")

    val compiledSouffleProg = new CompiledProgram {
      override def irModules: Seq[Module] = generateMods
      override def createCompiledUnit(modules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean): CompiledUnit =
        CompiledSouffleModule(modules, otherUnits, false, modules.head.name)
    }
    compiledSouffleProg.setPipeline(souffle_pipeline)

    print(compiledSouffleProg.irModules)

    val code = FileUtil.readFileFromResource("functional/interop/interop.finca")
    val options = FunctionalCompilerOptions.default
    options.irLogging.logModule = true
    options.irLogging.logLowerings = true
    val exec: FunctionalExecutor = new FunctionalExecutor(inca.viatra.backend.Executor())
    val compiled = exec.compileFunction(code, options, compiledSouffleProg.compiledUnits)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
  }
