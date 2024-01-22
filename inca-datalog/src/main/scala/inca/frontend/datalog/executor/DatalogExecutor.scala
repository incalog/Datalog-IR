package inca.frontend.datalog.executor

import inca.frontend.datalog.syntax.Parser
import inca.frontend.datalog.compile.{CompiledDatalogModule, DatalogCompilerOptions}
import inca.ir.execution.{IRExecutor, Relation}

import scala.annotation.targetName

object DatalogExecutor:
  @targetName("wildcard")
  val ? : Null = null

class DatalogExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledDatalogModule):
    val logAllRelations: Boolean = compiled.compilerOptions.datalogLogging.verboseOutput

    def output(rel: Relation): Relation = {
      if (logAllRelations)
        engine.readAll().foreach { r => println(r.asTable) }
      engine.read(rel)
    }

    def query(rel: String, tups: Seq[Any]*): Relation = {
      val inputRel = compiled.ir.relations.get(rel) match
        case Some(r) =>
          if (tups.exists(t => r.params.size != t.size))
            throw IllegalArgumentException(s"Each tuple should have size ${r.params.size} in $tups")
          Relation.from(rel, r.params.map(_.name.name), tups)
        case None =>
          throw IllegalStateException(s"No relation found for name $rel")
      output(inputRel)
    }

  def loadDatalog(compiled: CompiledDatalogModule): Loaded = {
    val engine = exec.instantiate(compiled)
    Loaded(engine, compiled)
  }

  def compileDatalog(code: String, compilerOptions: DatalogCompilerOptions): CompiledDatalogModule = {
    val module = Parser.parseModule(code)
    CompiledDatalogModule(module, compilerOptions)
  }
