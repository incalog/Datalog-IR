package inca.frontend.oodl.executor

import inca.frontend.oodl.compile.GenerateIR.{castRelationName, extensionalRelationName}
import inca.frontend.oodl.compile.{CompiledOODLUnit, GenerateIR, OODLCompilerOptions}
import inca.frontend.oodl.syntax.*
import inca.ir
import inca.ir.execution.{IRExecutor, Relation, UnitRelation}

import scala.jdk.CollectionConverters.*

case class TypeCastException(message: String) extends Exception(message, null)

class OODLExecutor(val exec: IRExecutor):
  case class Loaded(engine: exec.Engine, compiled: CompiledOODLUnit):
    val verboseOutput: Boolean = compiled.compilerOptions.oodlLogging.verboseOutput

    private def throwTypeCastExceptionIfRequired(): Unit = {
      def flattenAndProject(rel: Relation, size: Int): Set[Seq[AnyRef]] = {
        val projectedRel = rel.project(0, size)
        projectedRel.toSet.map(t => projectedRel.flattenEntry(t))
      }

      val allRelations = engine.readAll().map(r => r.name -> r).toMap
      val castRelOption = allRelations.get(castRelationName)
      val castInputRelOption = allRelations.get(s"$castRelationName$$input")
      (castRelOption, castInputRelOption) match {
        case (Some(castRel), Some(castInputRel)) =>
          val size = castInputRel.arity.min(castRel.arity)
          if (castInputRel.arity != castRel.arity)
            println(s"Warning: Can not reliably detect cast errors, since \"${castRel.name}\" and \"${castInputRel.name}\" have different arity.")
          val diff = flattenAndProject(castInputRel, size).diff(flattenAndProject(castRel, size))
          diff.foreach {
            case List(obj, ty) => throw TypeCastException(s"Can not cast object $obj to type $ty")
            case obj => throw TypeCastException(s"Unexpected cast entry: $obj")
          }
        case _ => // nothing
      }
    }

    def output(pat: String, tuple: Seq[Any]): Relation = {
      if (verboseOutput)
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

  def loadOODL(compiled: CompiledOODLUnit): Loaded = {
    // TODO: use correct DataModel
    val engine = exec.instantiate(compiled)
    Loaded(engine, compiled)
  }

  def compileOODL(code: String, compilerOptions: OODLCompilerOptions): CompiledOODLUnit = {
    val module = Parser.parseModule(code)
    CompiledOODLUnit(module, compilerOptions)
  }
