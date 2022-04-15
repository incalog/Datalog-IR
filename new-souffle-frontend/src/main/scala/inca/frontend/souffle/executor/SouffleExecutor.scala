package inca.frontend.souffle.executor

import inca.compiler.Options
import inca.frontend.souffle.{Parser, PrettyPrinter, Syntax}
import inca.frontend.souffle.compiler.Compiler
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.inputreader.InputToNamedRelationsReader
import inca.runtime.{EnginePool, Query}
import inca.runtime.context.QueryScope
import inca.runtime.db.{Database, DatabaseInput}
import inca.util.Scala.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory

import scala.jdk.CollectionConverters.CollectionHasAsScala

object SouffleExecutor {
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledSouffleModule) {
    lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler

    lazy val loadedPsystemModule: String = scalaCompiler.define {
      import scala.meta._
      q"object O {..${compiled.psystemSource.stats}}".syntax
    }

    def printMatches(name: String): Unit = {
      val matcher = engine.getMatcher(compiled.psystemModule.patterns(name)())
//      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
    }

    def printAllMatches(): Unit = {
      compiled.psystemModule.patterns.keys.foreach(printMatches)
    }

    def output(pat: String): Results[AnyRef] = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      val outputMatches = mainMatcher.getAllMatches.asScala.map(_.toArray.toSeq).toSeq
      new Results(outputMatches)
    }

    def sizeOfRelation(pat: String): Int = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      mainMatcher.getAllMatches.size()
    }

    type Outputs = Map[String, Results[AnyRef]]

    def execute(dir: String): Outputs = {
      val inputReader = new InputToNamedRelationsReader(dir)
      val dbInput: DatabaseInput = inputReader.compile(compiled.inputs.values.toMap)

      // TODO compiled.outputs, correctly storing outputs is your task
      // val outputDirectives: Seq[String] = Seq()
      val outputDirectives: Seq[String] =
        compiled.outputs.keys.toSeq

      var outputs: Outputs = Map()
      var sizes: Map[String, Int] = Map()
      engine.delayUpdatePropagation { () =>
        feed.processDatabaseInput(dbInput)
      }

      // get sizes of relations with .printsize directive
      compiled.printSizes.foreach { decl =>
        val size = sizeOfRelation(decl.name)
        sizes = sizes + (decl.name -> size)
      }

      // get relation content of relations with .output directive
      outputs = outputDirectives.map { rel =>
        rel -> output(rel)
      }.toMap

      sizes.foreach { case (rel, size) =>
        println(s"size of ${rel} is $size")
      }

      outputs
    }
  }


  class Results[T](val res: Seq[Seq[T]]) {
    override def equals(obj: Any): Boolean = obj match {
      case expected: Results[T] =>
        res.size == expected.res.size &&
          res.forall(ac => expected.res.exists(ex => sameVals(ac, ex))) &&
          expected.res.forall(ex => res.exists(ac => sameVals(ac, ex)))
      case _ => false
    }

    private def sameVals(actual: Seq[T], expected: Seq[T]): Boolean = actual.size == expected.size &&
      actual.zip(expected).forall{ case (x,y) => x == y }

    override def toString: String = s"Results(${res.mkString(", ")})"
  }


  def compileFunction(code: String, options: Options = Options()): CompiledSouffleModule = {
    val parsed = Parser.parse(code)
    val compiler = new Compiler()
    compiler.compileProgram(parsed)
  }

  def loadFunction(compiled: CompiledSouffleModule): Loaded = {
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, DRedReteBackendFactory.INSTANCE)
    Loaded(engine, feed, compiled)
  }

  def loadFunction(code: String, options: Options = Options()): Loaded =
    loadFunction(compileFunction(code, options))
}
