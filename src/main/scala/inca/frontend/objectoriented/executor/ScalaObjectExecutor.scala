package inca.frontend.objectoriented.executor

import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}
import inca.frontend.objectoriented.lowering.GenerateScala
import inca.util.Scala.ScalaCompiler
import sourcecode.Text.generate

import scala.collection.immutable.ArraySeq
import scala.meta.XtensionQuasiquoteTerm

object ScalaObjectExecutor extends Executor {
  private val genScala = new GenerateScala

  case class ScalaLoaded(compiled: CompiledObjectModule) extends Loaded {
    lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler
    lazy val genScala: GenerateScala = new GenerateScala

    def execute(main: String, args: Seq[meta.Term], deleteInput: Boolean = false): Results[Any] = {
      val mod = genScala.genModule(compiled.typed)
      val components = main.split("\\$")
      if (components.size != 2) {
        throw new IllegalArgumentException("Main must have the format {Object}.{MainMethod}")
      }
      val (obj, method) = mod.main(components(0), components(1))
      val code = s"""{
        ${mod.source.syntax}
        ${obj.name}.${method.name}(${args.mkString(",")})
      }"""//.stripMargin

      println(code)

      // convert the result to the expected format that a datalog query produces
      scalaCompiler.compileAndLoadScala[Any](code) match {
        case s: Set[_] =>
          results(s.map {
            case p: Product =>  ArraySeq.from(shapeless(p))
            case e => Seq(e)
          }.toSeq)
        case p: Product => results(Seq(ArraySeq.from(shapeless(p))))
        case _: Unit =>  results(Seq(Seq()))
        case e => resultVal(e)
      }
    }
  }

  private def shapeless(tup: Product): Seq[Any] = tup.productIterator.flatMap {
    case s: Product => shapeless(s)
    case e => Seq(e)
  }.toSeq

  def compileObject(code: String, options: ObjectOptions = ObjectOptions()): CompiledObjectModule = {
    Compiler.compileObject(code, options)
  }

  def loadFunction(compiled: CompiledObjectModule): Loaded = {
    ScalaLoaded(compiled)
  }

  def loadFunction(code: String, options: ObjectOptions = ObjectOptions()): Loaded =
    loadFunction(compileObject(code, options))
}
