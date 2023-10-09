package inca.frontend.functional.executor

import inca.ir
import inca.frontend.functional.compile.GenerateDatalog.extensionalRelationName
import inca.frontend.functional.compile.{CompiledFunctionalModule, GenerateDatalog}
import inca.util.ScalaCompiler
import inca.frontend.functional.syntax.*
import inca.ir.execution.{IRExecutor, Relation}
import inca.viatra.runtime.context.{DataModel, QueryScope}
import inca.viatra.runtime.db.{DBValue, Database, DatabaseInspector}
import inca.viatra.runtime.{EnginePool, Query}

import scala.jdk.CollectionConverters.*

class FunctionalExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledFunctionalModule):
    //lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler

    def output(pat: String, tuple: Seq[Any]): Relation = {
      val rel = engine.read(pat)
      rel.project(tuple.size, Int.MaxValue)
    }

    def execute(main: String, args: Seq[Any]): Relation = {
      engine.insert(extensionalRelationName(main), args)
      output(main, args)
    }

  def loadFunction(compiled: CompiledFunctionalModule): Loaded = {
    // TODO: use correct DataModel
    val engine = exec.instantiate(compiled)
    Loaded(engine, compiled)
  }

  def compileFunction(code: String): CompiledFunctionalModule = {
    val module = Parser.parseModule(code)
    CompiledFunctionalModule(module)
  }
