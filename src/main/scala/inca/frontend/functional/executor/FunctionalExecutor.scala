//package inca.frontend.functional.executor
//
//import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
//import inca.compiler.{CompiledModule, Compiler}
//import inca.frontend.functional.compiler.FunctionalOptions
//import inca.runtime.context.QueryScope
//import inca.runtime.data.DataURI
//import inca.runtime.db.Database
//import inca.runtime.{EnginePool, Query}
//import inca.util.Scala.ScalaCompiler
//import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
//import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
//import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
//import truechange._
//import truediff.Diffable
//
//import scala.jdk.CollectionConverters._
//
//object FunctionalExecutor {
//  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledModule) {
//
//    lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler
//
//    // if lastArgs is Some then there is at least one entry in lastTuple
//    var lastArgs: Option[Seq[Any]] = None
//    // maps from fuction name to inserted tuple
//    var lastTuple: Map[String, Tuple] = Map()
//
//    lazy val loadedPsystemModule: String = scalaCompiler.define {
//      import scala.meta._
//      q"object O {..${compiled.psystemSource.stats}}".syntax
//    }
//
//    def printMatches(name: String): Unit = {
//      val matcher = engine.getMatcher(compiled.psystemModule.patterns(name)())
//      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
//    }
//
//    def printAllMatches(): Unit = {
//      compiled.psystemModule.patterns.keys.foreach(printMatches)
//    }
//
//
//    type Input = (EditScript, Tuple)
//
//    def input(args: Seq[meta.Term]): Input = {
//      lastArgs match {
//        case Some(lastArgs1) =>
//          val (ess, cargs, updatedArgs) = vals(args:_*).zip(lastArgs1).map {
//            case (newArg: Diffable, oldArg: Diffable) =>
//              val (edits, updatedArg) = oldArg.compareTo(newArg)
//              (edits, updatedArg.uri, updatedArg)
//            case (litnew, litold) => (EditScript(Seq()), litnew, litnew)
//          }.unzip3
//          lastArgs = Some(updatedArgs)
//          (EditScript(ess.flatMap(_.edits)), Tuples.flatTupleOf(cargs:_*))
//        case None =>
//          val (ess, cargs, updatedArgs) = vals(args:_*).map {
//            case arg: Diffable => (arg.loadEdits, arg.uri, arg)
//            case lit => (EditScript(Seq()), lit, lit) }.unzip3
//          lastArgs = Some(updatedArgs)
//          (EditScript(ess.flatMap(_.edits)), Tuples.flatTupleOf(cargs:_*))
//      }
//    }
//
//    def input(arg: meta.Term): Input = input(Seq(arg))
//
//    def output(pat: String, tuple: Tuple): Results[AnyRef] = {
//      val mainSpec = compiled.psystemModule.patterns(pat)()
//      val mainMatcher = engine.getMatcher(mainSpec)
//      val arity = mainMatcher.getParameterNames.size()
//      val inputSeq = tuple.getElements ++ (for (_ <- 0 until (arity - tuple.getSize)) yield null)
//      val inputMatch = Query.Match(mainSpec, inputSeq, isMutable = false)
//      val outputMatches = mainMatcher.getAllMatches(inputMatch).asScala.map { m =>
//        m.toArray.slice(tuple.getSize, arity).toSeq
//      }.toSeq
//      new Results(outputMatches, -1, -1)
//    }
//
//    def countTuples(pat: String): Int = {
//      val mainSpec = compiled.psystemModule.patterns(pat)()
//      val mainMatcher = engine.getMatcher(mainSpec)
//      mainMatcher.countMatches()
//    }
//
//    def countTuples(pat: String, tuple: Tuple): Int = {
//      val mainSpec = compiled.psystemModule.patterns(pat)()
//      val mainMatcher = engine.getMatcher(mainSpec)
//      val partialMatch = Query.Match(mainSpec, tuple.getElements, isMutable = false)
//      mainMatcher.countMatches(partialMatch)
//    }
//
//
//    def measureInput(args: meta.Term*): (EditScript, Tuple) = {
//      val (ess, cargs) = vals(args:_*).map {
//        case arg: Diffable => (arg.loadEdits, arg.uri)
//        case lit => (EditScript(Seq()), lit)
//      }.unzip
//      (EditScript(ess.flatMap(_.edits)), Tuples.flatTupleOf(cargs: _*))
//    }
//
//    def measure(main: String, args: Seq[meta.Term]): (Long, Long, Long) = {
//      val (es, tuple) = measureInput(args:_*)
//      measure(main, es, tuple)
//    }
//
//    def measure(main: String, edits: EditScript, tuple: Tuple): (Long, Long, Long) = {
//      val mainSpec = compiled.psystemModule.patterns(main)()
//      val mainMatcher = engine.getMatcher(mainSpec)
//
//      val startLoadDB = System.nanoTime()
//      engine.delayUpdatePropagation { () => feed.processEditScript(edits) }
//      val endLoadDB = System.nanoTime()
//      val loadingTime = endLoadDB - startLoadDB
//
//      val startInsertQuery = System.nanoTime()
//      feed.insert(demandPatternExtensionalPrefix + main, tuple)
//      val endInsertQuery = System.nanoTime()
//
//      (loadingTime, endInsertQuery - startInsertQuery, -1)
//    }
//
//
//    def execute(main: String, args: Seq[meta.Term], deleteInput: Boolean = false): Results[AnyRef] = {
//      // TODO need to check if tuple is different (if it is the case insert new and delete old, else only process editscript)
//      executeInput(main, input(args), deleteInput)
//    }
//
//    def executeInput(main: String, input: Input, deleteInput: Boolean = false): Results[AnyRef] = {
//      val (es, tuple) = input
//      engine.delayUpdatePropagation { () =>
//        feed.processEditScript(es)
//        lastTuple.get(main) match {
//          case Some(oldTuple) =>
//            // check if last and current tuple are equal
//            if (oldTuple != tuple) {
//              feed.insert(demandPatternExtensionalPrefix + main, tuple)
//            } else {
//              // do nothing tuples are the same
//            }
//          case None =>
//            feed.insert(demandPatternExtensionalPrefix + main, tuple)
//        }
//        lastTuple = lastTuple + (main -> tuple)
//      }
//      val result = output(main, tuple)
//      if (deleteInput) {
//        feed.delete(demandPatternExtensionalPrefix + main, tuple)
//      }
//      result
//    }
//
//    def vals(ts: meta.Term*): Seq[AnyRef] = {
//      ts.map(a => {
//        val syntax = s"{import ${loadedPsystemModule}.${compiled.name}._; ${a.syntax}}"
//        scalaCompiler.compileAndLoadScala[AnyRef](syntax)
//      })
//    }
//
//    def results[T](res: Seq[Seq[T]]): Results[T] = new Results(res, -1, -1)
//    def resultVal[T](res: T): Results[T] = results(Seq(Seq(res)))
//    def result(res: meta.Term*): Results[AnyRef] = results(Seq(vals(res:_*)))
//  }
//
//  class Results[T](val res: Seq[Seq[T]], var timeInsertNano: Long, var timeDeleteNano: Long) {
//    override def equals(obj: Any): Boolean = obj match {
//      case expected: Results[T] =>
//        res.size == expected.res.size &&
//          res.forall(ac => expected.res.exists(ex => sameVals(ac, ex))) &&
//          expected.res.forall(ex => res.exists(ac => sameVals(ac, ex)))
//      case _ => false
//    }
//
//    private def sameVals(actual: Seq[T], expected: Seq[T]): Boolean = actual.size == expected.size &&
//      actual.zip(expected).foldLeft(true) {
//        case (true, (u1: DataURI, u2: DataURI)) => u1.repr == u2.repr
//        case (true, (u1: DataURI, u2: Diffable)) if u2.uri.isInstanceOf[DataURI] => u1.repr == u2.uri.asInstanceOf[DataURI].repr
//        case (true, (u1, u2)) => u1 == u2
//        case _ => false
//      }
//
//    override def toString: String = s"Results(${res.mkString(", ")})"
//  }
//
//  def compileFunction(code: String): CompiledModule = {
//    val options = FunctionalOptions()
//    Compiler.compileFunctional(code, options)
//  }
//
//  def loadFunction(compiled: CompiledModule): Loaded = {
//    val scope = new QueryScope(compiled.dataModel)
//    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//    Loaded(engine, feed, compiled)
//  }
//
//  def loadFunction(code: String): Loaded =
//    loadFunction(compileFunction(code))
//}
package inca.frontend.functional.executor

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.{CompiledModule, Compiler}
import inca.frontend.functional.compiler.{CompiledFunctionalModule, FunctionalOptions}
import inca.frontend.ir.Relation
import inca.runtime.Query.Specification
import inca.runtime.context.QueryScope
import inca.runtime.db.{DBValue, Database, DatabaseInspector}
import inca.runtime.{EnginePool, Query}
import inca.util.Scala.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.EditScript
import truediff.Diffable

import scala.jdk.CollectionConverters._

object FunctionalExecutor {
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledFunctionalModule) {
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


    type Input = (EditScript, Tuple)

    def input(arg: meta.Term): Input = input(Seq(arg))

    def input(args: Seq[meta.Term]): Input = {
      val (ess, cargs, _) = vals(args:_*).map {
        case arg: Diffable => (arg.loadEdits, arg.uri, arg)
        case lit => (EditScript(Seq()), lit, lit)
      }.unzip3
      (EditScript(ess.flatMap(_.edits)), Tuples.flatTupleOf(cargs:_*))
    }


    def output(pat: String, tuple: Tuple): Results[AnyRef] = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      val arity = mainMatcher.getParameterNames.size()
      val inputSeq = tuple.getElements ++ (for (_ <- 0 until (arity - tuple.getSize)) yield null)
      val inputMatch = Query.Match(mainSpec, inputSeq, isMutable = false)

      compiled.psystemModule.patterns.foreach { case (k, p) =>
        val specification: Specification = p()
        val matcher = specification.getMatcher(engine)
        val rel = Relation.fromMatcher(matcher)
        println(rel.asTable)
        println()
      }

      val outputMatches = mainMatcher.getAllMatches(inputMatch).asScala.map { m =>
        m.toArray.slice(tuple.getSize, arity).toSeq
      }.toSeq
      new Results(outputMatches)
    }

    def execute(main: String, args: Seq[meta.Term], deleteInput: Boolean = false): Results[AnyRef] =
      executeInput(main, input(args), deleteInput)

    def executeInput(main: String, input: Input, deleteInput: Boolean = false): Results[AnyRef] = {
      val (es, tuple) = input
      feed.processEditScript(es)
      feed.insert(extensionalRelationName(main, tuple), tuple)
      val results = output(main, tuple)
      if (deleteInput)
        feed.delete(extensionalRelationName(main, tuple), tuple)
      results
    }

    def extensionalRelationName(main: String, tuple: Tuple): String = {
      val numBoundParams = tuple.getSize
      val boundAdorn = (0 until numBoundParams).map(_ => "b")
      val mainPattern = compiled.optimized.pats.find(_.name == main).getOrElse(throw new IllegalArgumentException(s"WHY $main not exist?"))
      val numFreeParams =  mainPattern.params.size - numBoundParams
      val freeAdorn = (0 until numFreeParams).map(_ => "f")
      val name = demandPatternExtensionalPrefix + main + "$" + (boundAdorn ++ freeAdorn).mkString
      name
    }

    def measure(main: String, args: Seq[meta.Term]): (Long, Long, Long) = {
      val (es, tuple) = input(args)
      measure(main, es, tuple)
    }

    def measure(main: String, edits: EditScript, tuple: Tuple): (Long, Long, Long) = {
      val mainSpec = compiled.psystemModule.patterns(main)()
      val mainMatcher = engine.getMatcher(mainSpec)

      val startLoadDB = System.nanoTime()
      engine.delayUpdatePropagation { () => feed.processEditScript(edits) }
      val endLoadDB = System.nanoTime()
      val loadingTime = endLoadDB - startLoadDB

      val startInsertQuery = System.nanoTime()
      feed.insert(extensionalRelationName(main, tuple), tuple)
      val endInsertQuery = System.nanoTime()

      println(s"Tuples in $main: ${mainMatcher.getAllMatches().size()}")

      (loadingTime, endInsertQuery - startInsertQuery, -1)
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

    def printResult(res: Results[AnyRef]): Unit = {
      val db = DatabaseInspector(feed)
      res.res.foreach { tuple =>
        val tupleStrings = tuple.map { v => DBValue.prettyPrint(v, db) }
        println(tupleStrings.mkString(", "))
      }
    }
  }


  class Results[T](val res: Seq[Seq[T]]) {
    def isEmpty: Boolean = res.isEmpty

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


  def compileFunction(code: String, options: FunctionalOptions = FunctionalOptions()): CompiledFunctionalModule = {
    Compiler.compileFunctional(code, options)
  }

  def loadFunction(compiled: CompiledFunctionalModule): Loaded = {
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }

  def loadFunction(code: String, options: FunctionalOptions = FunctionalOptions()): Loaded =
    loadFunction(compileFunction(code, options))
}
