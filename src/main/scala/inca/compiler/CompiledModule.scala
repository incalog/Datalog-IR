package inca.compiler

import inca.backend.analyze.StratificationAnalysis
import inca.backend.ir.{GeneratePSystem, IR, PSystem}
import inca.runtime.context.DataModel
import inca.util.Scala
import inca.util.TupleOps.transClosure

import scala.collection.immutable.MultiDict
import scala.collection.mutable.ListBuffer

trait CompiledModule {
  val options: Options
  def name: IR.Name
  def sourceLocation: SourceLocation

  def ir: IR.Module
  def dataModel: DataModel

  lazy val patternDependencies: MultiDict[IR.Name, IR.Name] = {
    var deps = MultiDict[IR.Name, IR.Name]()
    for (pat <- ir.pats;
         body <- pat.bodies;
         atom <- body.atoms) atom match {
      case IR.Call(trg, _, _, _) => deps += pat.name -> trg
      case _ => // nothing
    }
    deps
  }

  lazy val patternDependenciesTrans: MultiDict[IR.Name, IR.Name] = transClosure(patternDependencies)

  def printStatistics(): Unit = {
    val pats = optimized.pats.filter(!_.name.contains("oalesced"))
    println(s"GP relations: ${pats.size}")
    println(s"GP bodies: ${pats.map(_.bodies.size).sum}")
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

  lazy val transformed: IR.Module = {
    var module = ir
    for (trans <- options.transformations) {
      module = trans.transformer(dataModel).transformModule(module)
      if (CompilerFlags.DEBUGMODE) {
        println(s"Transformation: ${trans.getClass.getName}")
        println(module)
      }
    }
    module
  }

  lazy val analyzed: IR.Module = {
    StratificationAnalysis.analyze(transformed)
    transformed
  }

  lazy val optimized: IR.Module = {
    var module = analyzed
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
    val source = GeneratePSystem.compileModule(optimized)(Map())
    if (CompilerFlags.DEBUGMODE) {
      println(s"PSystem")
      println(source.syntax)
    }
    source
  }

  lazy val psystemModule: PSystem.Module = {
    import scala.meta._
    val loadSource = source"..${psystemSource.stats}; ${Term.Name(name)}"
    Scala.compileAndLoadScala[PSystem.Module](loadSource.syntax)()
  }
}

object CompiledModule {
  case class Failed(module: CompiledModule, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
}