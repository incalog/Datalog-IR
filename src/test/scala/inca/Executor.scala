package inca

import inca.compiler.{CompiledModule, Compiler, Options}
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.data.DataURI
import inca.runtime.{Database, EnginePool, Query}
import inca.util.Meta.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

import scala.jdk.CollectionConverters._

object Executor {
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledModule) {
    lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler

    val loadedPsystemModule: String = scalaCompiler.define {
      import scala.meta._
      q"object O {..${compiled.psystemSource.stats}}".syntax
    }

    def printMatches(name: String): Unit = {
      val matcher = engine.getMatcher(compiled.psystemModule.patterns(name)())
      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
    }
    def printAllMatches(): Unit = {
      compiled.psystemModule.patterns.keys.foreach(printMatches)
    }

    def execute(main: String, args: Seq[meta.Term], deleteInput: Boolean = false): Results[AnyRef] = {
      val cargs = vals(args:_*).map {
        case arg: Diffable =>
          feed.processEditScript(Diffable.load(arg))
          arg.uri
        case lit => lit
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

      if (deleteInput)
        feed.delete(s"ext_input_$main", tuple)

      new Results(outputMatches)
    }

    def vals(ts: meta.Term*): Seq[AnyRef] = {
      ts.map(a => {
        val syntax = s"{import ${loadedPsystemModule}._; ${a.syntax}}"
        scalaCompiler.compileAndLoadScala[AnyRef](syntax)
      })
    }

    def results[T](res: Seq[Seq[T]]): Results[T] = new Results(res)
    def resultVals[T](res: T*): Results[T] = results(Seq(res))
    def resultVal[T](res: T): Results[T] = results(Seq(Seq(res)))
    def result(res: meta.Term*): Results[AnyRef] = results(Seq(vals(res:_*)))
  }


  class Results[T](val res: Seq[Seq[T]]) {
    override def equals(obj: Any): Boolean = obj match {
      case expected: Results[T] =>
        res.zip(expected.res).foldLeft(true) { case (b, (r, e)) => b && sameVals(r, e) }
      case _ => false
    }

    private def sameVals(actual: Seq[T], expected: Seq[T]): Boolean = actual.size == expected.size &&
      actual.zip(expected).foldLeft(true) {
        case (true, (u1: DataURI, u2: DataURI)) => u1.repr == u2.repr
        case (true, (u1: DataURI, u2: Diffable)) if u2.uri.isInstanceOf[DataURI] => u1.repr == u2.uri.asInstanceOf[DataURI].repr
        case (true, (u1, u2)) => u1 == u2
        case _ => false
      }

    override def toString: String = s"Results(${res.mkString(", ")})"
  }

  def loadFunction(code: String, lmi: LanguageMetaInfo): Loaded = {
    val options = Options(lmi, transformations = Options.defaultTransformations)
    val compiled = Compiler.compileFun(code, options)
    val scope = new QueryScope(lmi)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }
}
