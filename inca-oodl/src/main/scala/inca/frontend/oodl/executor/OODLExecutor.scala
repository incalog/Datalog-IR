package inca.frontend.oodl.executor

import inca.frontend.oodl.compile.GenerateIR.{castRelationName, extensionalRelationName}
import inca.frontend.oodl.compile.{CompiledOODLModule, GenerateIR}
import inca.frontend.oodl.syntax.*
import inca.ir
import inca.ir.execution.{IRExecutor, Relation, UnitRelation}

import scala.jdk.CollectionConverters.*

case class TypeCastException(message: String) extends Exception(message, null)

class OODLExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledOODLModule):

    private def throwTypeCastExceptionIfRequired(): Unit = {
      def flattenAndProject(rel: Relation): Set[Seq[AnyRef]] = {
        val projectedRel = rel.project(0, 2)
        projectedRel.toSet.map(t => projectedRel.flattenEntry(t))
      }

      val allRelations = engine.readAll().map(r => r.name -> r).toMap
      val castRelOption = allRelations.get(castRelationName)
      val castInputRelOption = allRelations.get(s"$castRelationName$$input")
      (castRelOption, castInputRelOption) match {
        case (Some(castRel), Some(castInputRel)) =>
          val diff = flattenAndProject(castInputRel).diff(flattenAndProject(castRel))
          diff.foreach {
            case List(obj, ty) => throw TypeCastException(s"Can not cast object $obj to type $ty")
            case d => throw IllegalStateException(s"Unexpected cast entry $d")
          }
        case _ => // nothing
      }
    }

    def output(pat: String, tuple: Seq[Any]): Relation = {
      engine.readAll().foreach { r => println(r.asTable) }
      val rel = engine.read(UnitRelation(pat))
      throwTypeCastExceptionIfRequired()
      rel.project(tuple.size, Int.MaxValue)
    }

    def execute(main: String, args: Seq[Any]): Relation = {
      // make sure the main function exists
      val mainFun = compiled.fun.functions.find(_.name.name == main)
      mainFun match
        case Some(fun) =>
          if (fun.params.size != args.size)
            throw IllegalArgumentException(s"Expected ${fun.params.size} arguments, but got ${args.size}")
        case _ => throw IllegalArgumentException(s"No function named $main found")

      val allocIn = 1
      val mutIn = 1
      val monoIn = 1
      val edbEntry = Relation.from(extensionalRelationName(main), args :+ allocIn :+ mutIn :+ monoIn)
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
    //println(module)
    CompiledOODLModule(module)
  }
