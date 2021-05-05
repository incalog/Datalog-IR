package inca.compiler

import inca.backend.ir.{CompileToPSystem, GP, PSystem}
import inca.frontend.constraint.parser.{CoreParser, SourceLocation}
import inca.runtime.context.DataModel
import inca.util.Meta
import inca.util.TupleOps.transClosure

import scala.collection.immutable.MultiDict
import scala.collection.mutable.ListBuffer

trait CompiledModule {
  val options: Options

  def dataModel: DataModel

  def name: GP.Name

  def sourceLocation: SourceLocation

  def ir: GP.Module

  lazy val patternDependencies: MultiDict[GP.Name, GP.Name] = {
    var deps = MultiDict[GP.Name, GP.Name]()
    for (pat <- ir.pats;
         body <- pat.bodies;
         con <- body.constraints) con match {
      case GP.Call(trg, _, _, _) => deps += pat.name -> trg
      case _ => // nothing
    }
    deps
  }

  lazy val patternDependenciesTrans: MultiDict[GP.Name, GP.Name] = transClosure(patternDependencies)

  def printStatistics(): Unit = {
    println(s"GP relations: ${ir.pats.size}")
    println(s"GP bodies: ${ir.pats.map(_.bodies.size).sum}")
    val recs = patternDependenciesTrans.sets.filter(p => p._2.contains(p._1))
    println(s"Recursive GP relations: ${recs.size}")
  }

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
    for (op <- options.optimizations) {
      module = op.optimizer(dataModel).optimizeModule(module)
      if (CompilerFlags.DEBUGMODE) {
        println(s"Optimization: ${op.getClass.getName}")
        println(module)
      }
    }
    module
  }

  lazy val psystemSource: meta.Source = {
    val source = CompileToPSystem.compileModule(optimized)(Map())
    if (CompilerFlags.DEBUGMODE) {
      println(s"PSystem")
      println(source.syntax)
    }
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