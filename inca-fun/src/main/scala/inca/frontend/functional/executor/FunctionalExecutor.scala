package inca.frontend.functional.executor

import inca.ir
import inca.frontend.functional.compile.GenerateIR.extensionalRelationName
import inca.frontend.functional.compile.{CompiledFunctionalModule, FunctionalCompilerOptions, GenerateIR}
import inca.frontend.functional.syntax.*
import inca.ir.execution.{IRExecutor, Relation, UnitRelation}

import scala.jdk.CollectionConverters.*

class FunctionalExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledFunctionalModule):
    val logAllRelations: Boolean = compiled.compilerOptions.funLogging.verboseOutput

    def output(pat: String, tuple: Seq[Any]): Relation = {
      if (logAllRelations)
        engine.readAll().foreach { r => println(r.asTable) }
      val rel = engine.read(UnitRelation(pat))
      rel.project(tuple.size, Int.MaxValue)
    }

    def execute(main: String, args: Seq[Any]): Relation = {
      if (args.nonEmpty)
        val edbEntry = Relation.from(extensionalRelationName(main), args)
        engine.insert(edbEntry)
      output(main, args)
    }

  def loadFunction(compiled: CompiledFunctionalModule): Loaded = {
    // TODO: use correct DataModel
    val engine = exec.instantiate(compiled)
    Loaded(engine, compiled)
  }

  def compileFunction(code: String, compilerOptions: FunctionalCompilerOptions): CompiledFunctionalModule = {
    val module = Parser.parseModule(code)
    CompiledFunctionalModule(module, compilerOptions)
  }
