package inca.ir

import inca.ir.extension.block
import inca.ir.typing.IRTypechecker
import inca.ir.util.SourceLocation
import inca.util.CompilationMessage

import scala.collection.immutable.MultiDict
import scala.collection.mutable.ListBuffer

trait CompiledModule {
  def name: Name
  def sourceLocation: SourceLocation

  def ir: Module

//  lazy val patternDependencies: MultiDict[Name, Name] = {
//    var deps = MultiDict[Name, Name]()
//    for (pat <- ir.pats;
//         body <- pat.bodies;
//         atom <- body.atoms) atom match {
//      case Datalog.Call(trg, _, _, _) => deps += pat.name -> trg
//      case _ => // nothing
//    }
//    deps
//  }

//  lazy val patternDependenciesTrans: MultiDict[Name, Name] = transClosure(patternDependencies)

  def printStatistics(): Unit = {
    val rels = ir.relations.values
    println(s"IR relations: ${rels.size}")
    println(s"IR bodies: ${rels.map(_.bodies.size).sum}")
//    val recs = patternDependenciesTrans.sets.filter(p => p._2.contains(p._1))
//    println(s"Recursive GP relations: ${recs.size}")
  }

  protected val messages: ListBuffer[CompilationMessage] = ListBuffer()
  def allMessages: List[CompilationMessage] = messages.toList
  def errors: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.ERROR).toList
  def warnings: List[CompilationMessage] = messages.filter(_.severity == CompilationMessage.WARNING).toList

  protected def stopIfNeeded(): Unit = {
    val es = errors
    val ws = es ++ warnings
    if (/*options.stopOnWarning &&*/ warnings.nonEmpty)
      throw CompiledModule.Failed(this, ws)
    if (/*options.stopOnError &&*/ errors.nonEmpty)
      throw CompiledModule.Failed(this, es)
  }

  lazy val checked: Module =
    val checker = new IRTypechecker
    val lowering = new block.Lowering {}
    val noBlockModule = lowering.lower(ir)
    try checker.typecheck(noBlockModule)
    finally println(noBlockModule)
    noBlockModule
}

object CompiledModule {
  case class Failed(module: CompiledModule, messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))
}