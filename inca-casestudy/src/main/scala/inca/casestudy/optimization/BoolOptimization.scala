package inca.casestudy.optimization

import inca.ir.{Body, Call, CompiledModule, Eq, Module, Name, Param, Relation, Term, Var, string2name, term2Arg}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.ir.extension.bool
import inca.ir.extension.block
import inca.ir.extension.disjunction
import inca.ir.extension.not
import inca.ir.execution.ThreadCount.Fixed

object BoolOptimization:
  def createCompiled(mod: Module, optimizeBool: Boolean): CompiledModule = new CompiledModule:
    override def name: Name = mod.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Module = mod

    override def compilerOptions: CompilerOptions = {
      val opt = CompilerOptions.default
      opt.irLogging.logLowerings = false
      opt.irLogging.logTypeInformation = false
      opt.irLogging.logStatsAfterOptimizations = false
      opt
    }

    val opts = if optimizeBool then
      List(() => new bool.Optimizer {})
    else
      List()

    setPipeline(opts ++ List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {}
    ))

  @main def runSimpleBoolUsingSouffle() = {
    val nestingLevels = 10
    val mod = Module("BoolTest", bool.IR.language, Seq(
      // We need this relation, since souffle does not do interrelational optimizations.
      // Note: This could also be an EDB input.
      Relation("input_main", Seq(
        Param("x", bool.TBoolean)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), bool.BoolTrue)
        ))
      )),
      Relation("main", Seq(
        Param("x", bool.TBoolean),
        Param("y", bool.TBoolean)
      ), Seq(
        Body(Seq(
          Call("input_main", Seq(Var("x"))),
          Eq(Var("y"),
            Range(0, nestingLevels).foldLeft[Term](Var("x")) { (acc, i) =>
              if i % 2 == 0 then
                bool.BoolAnd(bool.BoolTrue, acc)
              else
                bool.BoolOr(bool.BoolFalse, acc)
            }
          )
        )
      ))),

    ))

    println()
    println("---------------------")
    println("Module:")
    println("---------------------")
    println(mod)
    println()

    var compiled = createCompiled(mod, false)
    var engine = inca.souffle.backend.Executor(Fixed(1)).instantiate(compiled)
    println(engine.transformedRam())

    println()
    println("---------------------")
    println("With optimizations:")
    println("---------------------")
    println()

    compiled = createCompiled(mod, true)
    engine = inca.souffle.backend.Executor(Fixed(1)).instantiate(compiled)
    println(engine.transformedRam())
  }
