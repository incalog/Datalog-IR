package inca.integration

import inca.compiler.{CompiledModule, Compiler, Options}
import inca.examples.Code
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.{Database, EnginePool, Query}
import inca.util.Meta
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable

import scala.jdk.CollectionConverters._
import scala.meta.XtensionQuasiquoteTerm

class FunctionsTest extends AnyFunSuite {


  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledModule) {
    def printMatches(name: String): Unit = {
      val matcher = engine.getMatcher(compiled.psystemModule.patterns(name)())
      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
    }
    def printAllMatches(): Unit = {
      compiled.psystemModule.patterns.keys.foreach(printMatches)
    }
  }

  def loadFunction(code: String, lmi: LanguageMetaInfo): Loaded = {
    val options = Options(lmi, transformations = Options.defaultTransformations)
    val compiled = Compiler.compileFun(code, options)

    val scope = new QueryScope(lmi)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }

  def executeFunction(loaded: Loaded, main: String, args: Seq[meta.Term]): Seq[Seq[AnyRef]] = {
    val Loaded(engine, feed, compiled) = loaded

    val cargs = args.map(a => Meta.compileAndLoadScala[AnyRef](a.syntax)())
    cargs.foreach {
      case arg: Diffable =>
        feed.processEditScript(Diffable.load(arg))
      case _ => // nothing
    }

    val tuple = Tuples.flatTupleOf(cargs:_*)
    feed.insert(s"ext_input_$main", tuple)

    val mainSpec = compiled.psystemModule.patterns(main)()
    val mainMatcher = engine.getMatcher(mainSpec)
    val arity = mainMatcher.getParameterNames.size()
    val inputSeq = cargs ++ (for (_ <- 0 until (arity - cargs.size)) yield null)
    val inputMatch = Query.Match(mainSpec, inputSeq.toArray, isMutable = false)
    val outputMatches = mainMatcher.getAllMatches(inputMatch).asScala.map { m =>
      m.toArray.slice(cargs.size, arity).toSeq
    }.toSeq
    outputMatches
  }

  test("Factorial Example") {
    val fun = loadFunction(Code.factModule, new LanguageMetaInfo())
    assert(executeFunction(fun, "main_bf", Seq(q"5")) == Seq(Seq(120)))
    assert(executeFunction(fun, "main_bf", Seq(q"10")) == Seq(Seq(3628800)))
  }

  test("Fibonacci Example") {
    val fun = loadFunction(Code.fibModule, new LanguageMetaInfo())
    assert(executeFunction(fun, "main_bf", Seq(q"10")) == Seq(Seq(55)))
    assert(executeFunction(fun, "main_bf", Seq(q"11")) == Seq(Seq(89)))
    assert(executeFunction(fun, "main_bf", Seq(q"20")) == Seq(Seq(6765)))
  }
}
