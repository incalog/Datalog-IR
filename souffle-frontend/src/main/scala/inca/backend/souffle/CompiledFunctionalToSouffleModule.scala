package inca.backend.souffle

import inca.backend.souffle.GenerateFacts.EDB
import inca.compiler.source.SourceString
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import inca.frontend.functional.core.{DataDef, Module}
import inca.frontend.souffle.Syntax.Name
import inca.util.Scala.ScalaCompiler
import truechange.EditScript
import truediff.Diffable

object CompiledFunctionalToSouffleModule {
  def apply(src: String, options: FunctionalOptions = FunctionalOptions()): CompiledFunctionalToSouffleModule = {
    val fun = inca.frontend.functional.parser.Parser.parse(SourceString(src))
    new CompiledFunctionalToSouffleModule(fun, options)
  }
}
class CompiledFunctionalToSouffleModule(fun: Module, options: FunctionalOptions) extends CompiledFunctionalModule(fun, options) {
  // TODO check if module contains fold and abort
  lazy val compiler = new GenerateSouffle(dataModel)
  lazy val (souffleSource, inputRelations): (String, Seq[Name]) = {
    compiler.compileModule(optimized, fun.content.collect{case d: DataDef => d})
  }

  lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler

  val loadedPsystemModule: String = scalaCompiler.define {
    import scala.meta._
    q"object O {..${psystemSource.stats}}".syntax
  }
  def generateFacts(terms: Seq[meta.Term], mainRel: Name): EDB = {

    def vals(ts: meta.Term*): Seq[AnyRef] = {
      ts.map(a => {
        val syntax = s"{import ${loadedPsystemModule}.${name}._; ${a.syntax}}"
        scalaCompiler.compileAndLoadScala[AnyRef](syntax)
      })
    }


    val inputs = vals(terms:_*).map {
      case arg: Diffable => (arg.loadEdits, arg.uri)
      case lit => (EditScript(Seq()), lit)
    }
    val edits = inputs.flatMap(_._1.edits)
    val args = inputs.map(_._2)

    GenerateFacts(args, EditScript(edits), mainRel)
  }
}
