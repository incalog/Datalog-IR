package inca.frontend.objectoriented.executor

import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}
import inca.frontend.objectoriented.lowering.GenerateScala
import inca.util.Scala.ScalaCompiler

import scala.meta.XtensionQuasiquoteTerm

object ScalaObjectExecutor {
  private val genScala = new GenerateScala

  case class Loaded(compiled: CompiledObjectModule) {
    lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler
    lazy val genScala: GenerateScala = new GenerateScala

    def execute(main: String, args: Seq[meta.Term]): Any = {
      val mod = genScala.genModule(compiled.typed)
      val components = main.split("\\$")
      if (components.size != 2) {
        throw new IllegalArgumentException("Main must have the format {Class}.{MainMethod}")
      }
      val (obj, method) = mod.main(components(0), components(1))
      val code = s"""{
        ${mod.source.syntax}
        ${obj.name}.${method.name}(${args.mkString(",")})
      }""".stripMargin

      println(code)
      scalaCompiler.compileAndLoadScala[Int](code)
    }
  }


  def compileObject(code: String, options: ObjectOptions = ObjectOptions()): CompiledObjectModule = {
    Compiler.compileObject(code, options)
  }

  def loadFunction(compiled: CompiledObjectModule): Loaded = {
    Loaded(compiled)
  }

  def loadFunction(code: String, options: ObjectOptions = ObjectOptions()): Loaded =
    loadFunction(compileObject(code, options))
}
