package inca.frontend.functional.executor

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.{CompiledModule, Compiler}
import inca.frontend.functional.compiler.FunctionalOptions
import inca.runtime.context.QueryScope
import inca.runtime.db.{Database, DatabaseInspector}
import inca.runtime.{EnginePool, Query}
import inca.util.Scala.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.{AdvancedViatraQueryEngine, IMatchUpdateListener}
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import truechange.EditScript
import truediff.Diffable

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object IncrementalFunctionalExecutor {
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


    // TODO for fix implementation we assume that the inserted tuple remains the same (uris does not change of the outer most nodes)
    // Invariant: If you call input you need to use the input to update the analysis otherwise there will be inconsistent state
    var lastArgs: Option[Seq[Any]] = None
    var lastMainExtRel: Option[Tuple] = None

    type Input = (EditScript, Tuple)

    def input(arg: meta.Term): Input = input(Seq(arg))

    def input(args: Seq[meta.Term]): Input = {
      lastArgs match {
        case Some(last) =>
          val (ess, cargs, updatedArgs) = vals(args:_*).zip(last).map {
            case (newArg: Diffable, oldArg: Diffable) =>
              val (edits, updatedArg) = oldArg.compareTo(newArg)
              println(updatedArg.toStringWithURI)
              (edits, updatedArg.uri, updatedArg)
            case (litnew, _) => (EditScript(Seq()), litnew, litnew)
          }.unzip3
          lastArgs = Some(updatedArgs)
          (EditScript(ess.flatMap(_.edits)), Tuples.flatTupleOf(cargs:_*))
        case None =>
          val (ess, cargs, updatedArgs) = vals(args:_*).map {
            case arg: Diffable => (arg.loadEdits, arg.uri, arg)
            case lit => (EditScript(Seq()), lit, lit)
          }.unzip3
          lastArgs = Some(updatedArgs)
          (EditScript(ess.flatMap(_.edits)), Tuples.flatTupleOf(cargs:_*))
      }
    }


    def output(pat: String, tuple: Tuple): Results[Any] = {
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

    def execute(main: String, args: Seq[meta.Term], deleteInput: Boolean = false): Results[Any] =
      executeInput(main, input(args), deleteInput)

    def executeInput(main: String, input: Input, deleteInput: Boolean = false): Results[Any] = {
      val (es, tuple) = input
      feed.processEditScript(es)
      feed.insert(demandPatternExtensionalPrefix + main, tuple)
      val results = output(main, tuple)
      if (deleteInput)
        feed.delete(demandPatternExtensionalPrefix + main, tuple)
      results
    }

    def measureInitial(main: String, args: Seq[meta.Term]): (Long, Long, Long, Query.Matcher) = {
      val (es, tuple) = input(args)
      measureInitial(main, es, tuple)
    }

    def measureInitial(main: String, edits: EditScript, tuple: Tuple): (Long, Long, Long, Query.Matcher) = {
      val mainSpec = compiled.psystemModule.patterns(main)()
      val mainMatcher = engine.getMatcher(mainSpec)

      val startLoadDB = System.nanoTime()
      engine.delayUpdatePropagation { () => feed.processEditScript(edits) }
      val endLoadDB = System.nanoTime()
      val loadingTime = endLoadDB - startLoadDB

      val startInsertQuery = System.nanoTime()
      feed.insert(demandPatternExtensionalPrefix + main, tuple)
      val endInsertQuery = System.nanoTime()
      lastMainExtRel = Some(tuple)

      println(s"Tuples in $main: ${mainMatcher.getAllMatches().size()}")

      (loadingTime, endInsertQuery - startInsertQuery, -1, mainMatcher)
    }

    def measureUpdate(main: String, args: Seq[meta.Term]): (Long, Long, Long, Query.Matcher) = {
      val (es, tuple) = input(args)
      measureInitial(main, es, tuple)
    }

    def measureUpdate(main: String, edits: EditScript, tuple: Tuple): (Long, Long, Long, Query.Matcher) = {
      val mainSpec = compiled.psystemModule.patterns(main)()
      val mainMatcher = engine.getMatcher(mainSpec)

      val startQuery = System.nanoTime()
      engine.delayUpdatePropagation { () =>
        lastMainExtRel match {
          case Some(lastTuple) =>
            if (lastTuple != tuple) {
              feed.insert(demandPatternExtensionalPrefix + main, tuple)
              feed.delete(demandPatternExtensionalPrefix + main, lastTuple)
            } else {
              // do nothing
            }
          case None =>
            feed.insert(main, tuple)
        }
        feed.processEditScript(edits)
      }
      val endQuery = System.nanoTime()
      lastMainExtRel = Some(tuple)

      (-1, endQuery - startQuery, -1, mainMatcher)
    }

    def vals(ts: meta.Term*): Seq[Any] = {
      ts.map(a => {
        val syntax = s"{import ${loadedPsystemModule}.${compiled.name}._; ${a.syntax}}"
        scalaCompiler.compileAndLoadScala[Any](syntax)
      })
    }

    def results[T](res: Seq[Seq[T]]): Results[T] = new Results(res)
    def resultVals[T](res: T*): Results[T] = results(Seq(res))
    def resultVal[T](res: T): Results[T] = results(Seq(Seq(res)))
    def result(res: meta.Term*): Results[Any] = results(Seq(vals(res:_*)))

    // Functionality to track which tuples are inserted and removed
    private val changesInTrackedRelations: ListBuffer[(Query.Match, Boolean)] = ListBuffer()

    def registerTrackedRelations(rels: Set[String]): Unit =
      for (pat <- rels)
        engine.addMatchUpdateListener(
          engine.getMatcher(compiled.psystemModule.patterns(pat)()),
          new IMatchUpdateListener[Query.Match] {
            override def notifyAppearance(mtch: Query.Match): Unit =
              changesInTrackedRelations += ((mtch, true))
            override def notifyDisappearance(mtch: Query.Match): Unit =
              changesInTrackedRelations += ((mtch, false))
          },
          false
        )

    def printChanges(): Unit = {
      changesInTrackedRelations.foreach { case (m, ins) =>
        val direction =
          if (ins)
            Console.BLUE + "Insert"
          else
            Console.RED + "Remove"
        println(s"$direction $m" + Console.BLACK)
      }
      changesInTrackedRelations.clear()
    }

    def deepPrintChanges(): Unit = {
      val db = new DatabaseInspector(feed)
      changesInTrackedRelations.foreach { case (m, ins) =>
        val direction =
          if (ins)
            Console.BLUE + "Insert"
          else {
            Console.RED + "Remove"
          }
        val prettyPrint = try {
          m.deepPrettyPrint(db)
        } catch {
          case _: Exception => m.prettyPrint()
        }
        println(s"$direction ${m.spec.getSimpleName} ${prettyPrint}" + Console.BLACK)
      }
      changesInTrackedRelations.clear()
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

  def compileFunction(code: String, options: FunctionalOptions = FunctionalOptions()): CompiledModule = {
    Compiler.compileFunctional(code, options)
  }

  def loadFunction(compiled: CompiledModule): Loaded = {
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, compiled.options.mode)
    Loaded(engine, feed, compiled)
  }

  def loadFunction(code: String): Loaded =
    loadFunction(compileFunction(code))
}
