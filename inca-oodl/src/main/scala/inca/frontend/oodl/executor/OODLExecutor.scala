package inca.frontend.oodl.executor

import inca.frontend.oodl.compile.GenerateIR.extensionalRelationName
import inca.frontend.oodl.compile.{CompiledOODLModule, GenerateIR}
import inca.frontend.oodl.syntax.*
import inca.ir
import inca.ir.execution.{IRExecutor, Relation, UnitRelation}

import scala.jdk.CollectionConverters.*

class OODLExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledOODLModule):

    def output(pat: String, tuple: Seq[Any]): Relation = {
      val rel = engine.read(UnitRelation(pat))
      //engine.readAll().foreach { r => println(r.asTable) }
      rel.project(tuple.size, Int.MaxValue)
    }

    def execute(main: String, args: Seq[Any]): Relation = {
      val edbEntry = Relation.from(extensionalRelationName(main), args)
      engine.insert(edbEntry)
      output(main, args)
    }

  def loadOODL(compiled: CompiledOODLModule): Loaded = {
    // TODO: use correct DataModel
    val engine = exec.instantiate(compiled)
    Loaded(engine, compiled)
  }

  def compileOODL(code: String): CompiledOODLModule = {
    val module = Parser.parseModule(code)
    CompiledOODLModule(module)
  }
