package inca.frontend.datalog.executor

import inca.frontend.datalog.syntax.Parser
import inca.frontend.datalog.compile.CompiledDatalogModule
import inca.ir.execution.{IRExecutor, Relation}
import inca.util.compileroptions.CompilerOptions

import scala.annotation.targetName

object DatalogExecutor:
  @targetName("wildcard")
  val ? : Null = null

class DatalogExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledDatalogModule):

    def output(rel: Relation): Relation = {
      //engine.readAll().foreach { r => println(r.asTable) }
      engine.read(rel)
    }

    def query(rel: String, tups: Product*): Relation = {
      if (tups.isEmpty)
        throw IllegalArgumentException("Input tuple should not be empty")
      val inputRel = compiled.ir.relations.get(rel) match
        case Some(r) =>
          if (tups.forall(t => r.params.size != t.productArity))
            throw IllegalArgumentException(s"Each tuple should have size ${r.params.size}")
          Relation.from(rel, r.params.map(_.name.name), tups.map(_.productIterator.toSeq))
        case None =>
          throw IllegalStateException(s"No relation found for name $rel")
      output(inputRel)
    }

  def loadDatalog(compiled: CompiledDatalogModule): Loaded = {
    val engine = exec.instantiate(compiled)
    Loaded(engine, compiled)
  }

  def compileDatalog(code: String, compilerOptions: CompilerOptions): CompiledDatalogModule = {
    val module = Parser.parseModule(code)
    CompiledDatalogModule(module, compilerOptions)
  }
