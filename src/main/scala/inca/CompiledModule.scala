package inca

import inca.backend.ir.{CompileToPSystem, GP, PSystem}
import inca.frontend.parser.SourceLocation
import inca.util.Meta

import scala.collection.mutable.ListBuffer

trait CompiledModule {
  val compilerOptions: CompilerOptions

  def name: GP.Name
  def sourceLocation: SourceLocation

  def ir: GP.Module

  protected val messages: ListBuffer[CompilationMessage] = ListBuffer()
  def getMessages: List[CompilationMessage] = messages.toList
  def hasErrors: Boolean = messages.exists(_.severity == CompilationMessage.ERROR)

  lazy val optimized: GP.Module = {
    var module = ir
    // println(module)
    for (op <- compilerOptions.optimizations) {
      module = op.optimizer(compilerOptions.languageMetaInfo).optimizeModule(module)
    }
    // println(module)
    module
  }

  lazy val psystemSource: meta.Source =
    CompileToPSystem.compileModule(optimized)(Map())

  lazy val psystemModule: PSystem.Module = {
    import scala.meta._
    val loadSource = source"..${psystemSource.stats}; ${Term.Name(name)}"
    Meta.compileAndLoadScala[PSystem.Module](loadSource.syntax)()
  }
}


