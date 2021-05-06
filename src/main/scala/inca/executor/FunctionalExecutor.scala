package inca.executor

import inca.compiler.options.FunctionalOptions
import inca.compiler.{CompiledModule, Compiler}
import inca.runtime.context.QueryScope
import inca.runtime.data.DataURI
import inca.runtime.{Database, EnginePool, Query}
import inca.util.Meta.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

import scala.jdk.CollectionConverters._

object FunctionalExecutor {
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

    def input(arg: meta.Term): AnyRef = vals(arg) match {
      case Seq(arg: Diffable) =>
        feed.processEditScript(Diffable.load(arg))
        arg.uri
      case Seq(lit) => lit
    }

    def input(args: Seq[meta.Term]): Tuple = {
      val cargs = vals(args:_*).map {
        case arg: Diffable =>
          feed.processEditScript(Diffable.load(arg))
          arg.uri
        case lit => lit
      }
      Tuples.flatTupleOf(cargs:_*)
    }

    def output(pat: String, tuple: Tuple): Results[AnyRef] = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      val arity = mainMatcher.getParameterNames.size()
      val inputSeq = tuple.getElements ++ (for (_ <- 0 until (arity - tuple.getSize)) yield null)
      val inputMatch = Query.Match(mainSpec, inputSeq, isMutable = false)
      val outputMatches = mainMatcher.getAllMatches(inputMatch).asScala.map { m =>
        m.toArray.slice(tuple.getSize, arity).toSeq
      }.toSeq
      new Results(outputMatches)
    }


    def execute(main: String, args: Seq[meta.Term], deleteInput: Boolean = false): Results[AnyRef] =
      executeTuple(main, input(args), deleteInput)

    def executeTuple(main: String, tuple: Tuple, deleteInput: Boolean = false): Results[AnyRef] = {
      feed.insert(s"ext_input_$main", tuple)
      val results = output(main, tuple)
      if (deleteInput)
        feed.delete(s"ext_input_$main", tuple)
      results
    }

    def vals(ts: meta.Term*): Seq[AnyRef] = {
      ts.map(a => {
        val syntax = s"{import ${loadedPsystemModule}.${compiled.name}._; ${a.syntax}}"
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
        res.size == expected.res.size &&
          res.forall(ac => expected.res.exists(ex => sameVals(ac, ex))) &&
          expected.res.forall(ex => res.exists(ac => sameVals(ac, ex)))
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

  def loadFunction(code: String): Loaded = {
    val options = FunctionalOptions()
    val compiled = Compiler.compileFunctional(code, options)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }
}
