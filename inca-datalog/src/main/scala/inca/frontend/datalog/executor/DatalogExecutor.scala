package inca.frontend.datalog.executor

import inca.frontend.datalog.syntax.Parser
import inca.frontend.datalog.compile.CompiledDatalogModule
import inca.ir.execution.{IRExecutor, Relation}

object DatalogExecutor:
  // wildcard
  val __ : Null = null

class DatalogExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledDatalogModule):

    def output(rel: Relation): Relation = {
      //engine.readAll().foreach { r => println(r.asTable) }
      engine.read(rel)
    }

    def read(rel: String, arg: Product): Relation = {
      val inputRel = compiled.ir.relations.get(rel) match
        case Some(r) => Relation.from(rel, r.params.map(_.name.name), Seq(arg.productIterator.toSeq))
        case None => throw IllegalStateException(s"No relation found for name $rel")
      output(inputRel)
    }

  def loadDatalog(compiled: CompiledDatalogModule): Loaded = {
    val engine = exec.instantiate(compiled)
    Loaded(engine, compiled)
  }

  def compileDatalog(code: String): CompiledDatalogModule = {
    val module = Parser.parseModule(code)
    CompiledDatalogModule(module)
  }
