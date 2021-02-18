package inca.compiler

import inca.backend.ir.{CompileToPSystem, GP, PSystem}
import inca.frontend_old.parser.SourceLocation
import inca.util.Meta

import scala.collection.mutable.ListBuffer

trait CompiledModule {
  val options: Options

  def name: GP.Name

  def sourceLocation: SourceLocation

  def ir: GP.Module

  protected val messages: ListBuffer[CompilationMessage] = ListBuffer()
  def allMessages: List[CompilationMessage] = messages.toList
  def errors: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.ERROR).toList
  def warnings: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.WARNING).toList

  protected def stopIfNeeded(): Unit = {
    val es = errors
    val ws = es ++ warnings
    if (options.stopOnWarning && warnings.nonEmpty)
      throw CompiledModule.Failed(this, ws)
    if (options.stopOnError && errors.nonEmpty)
      throw CompiledModule.Failed(this, es)
  }

  lazy val optimized: GP.Module = {
    var module = ir
    // println(module)
    for (op <- options.optimizations) {
      module = op.optimizer(options.languageMetaInfo).optimizeModule(module)
//      println(op + "\n" + module.toString)
    }
    module
  }

  lazy val psystemSource: meta.Source = {
    val source = CompileToPSystem.compileModule(optimized)(Map())
//    println(source)
    source
  }

  lazy val psystemModule: PSystem.Module = {
    import scala.meta._
    val loadSource = source"..${psystemSource.stats}; ${Term.Name(name)}"
    Meta.compileAndLoadScala[PSystem.Module](loadSource.syntax)()
  }
}

object CompiledModule {
  case class Failed(module: CompiledModule, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
}