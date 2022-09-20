package inca.frontend.objectoriented.executor

import inca.backend.transform.magic.demand.DemandTransformation.{demandPatternExtensionalPrefix, demandPatternPrefix}
import inca.compiler.Compiler
import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}
import inca.frontend.objectoriented.lowering.GenerateDatalog.castPatName
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.data.ObjectID
import inca.runtime.db.{DBValue, Database, DatabaseInspector}
import inca.runtime.{EnginePool, Query}
import inca.util.Scala.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.{EditScript, SortType}
import truediff.Diffable

import scala.jdk.CollectionConverters._

object ObjectExecutor {
  case class TypeCastException(obj: ObjectID, typ: String) extends RuntimeException(s"Could not cast $obj to type $typ!")

  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledObjectModule) {
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
      feed.insert(demandPatternExtensionalPrefix + main, tuple)
      val results = output(main, tuple)
      throwTypeCastExceptionIfRequired()
      if (deleteInput)
        feed.delete(demandPatternExtensionalPrefix + main, tuple)
      results
    }

    def throwTypeCastExceptionIfRequired(): Unit = {
      // if no cast is used the relation is eliminated
      val hasPerformedCast = compiled.psystemModule.patterns.keys.exists(_ == castPatName)
      if (!hasPerformedCast)
        return

      val casts = output(castPatName, Tuples.flatTupleOf())
      val castInputs = output(demandPatternPrefix + castPatName, Tuples.flatTupleOf())

      // find a $cast match for each input$cast
      val castObjects = casts.res.toSet
      val castInputObjects = castInputs.res.toSet
      val diff = castInputObjects.diff(castObjects)
      if (diff.nonEmpty) {
        val firstFailure = diff.head
        val failureObj = firstFailure(0).asInstanceOf[ObjectID]
        val failureType = firstFailure(1).asInstanceOf[String]
        throw TypeCastException(failureObj, failureType)
      }
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
      feed.insert(demandPatternExtensionalPrefix + main, tuple)
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


  def compileObject(code: String, options: ObjectOptions = ObjectOptions()): CompiledObjectModule = {
    Compiler.compileObject(code, options)
  }

  def loadInheritanceEDB(dataModel: DataModel, feed: Database): Unit = {
    dataModel.types.foreach { typ =>
      feed.insert("subtype", Tuples.flatTupleOf(typ.name, typ.name))
    }
    dataModel.nodeSupertypes.foreach { case (child, parent) =>
      feed.insert("subtype", Tuples.flatTupleOf(child.name, parent.name))
    }

    // FIXME: This is only required as long as we don't have negation for ExtensionalCall
    val allTypes = dataModel.types
    allTypes.foreach { ty =>
      val tySupertypes = dataModel.nodeSupertypes.get(ty)
      (allTypes - ty).diff(tySupertypes).foreach { notSubtype =>
        feed.insert("not#subtype", Tuples.flatTupleOf(ty.name, notSubtype.name))
      }
    }
  }

  def loadFunction(compiled: CompiledObjectModule): Loaded = {
    val dataModel = compiled.dataModel
    val scope = new QueryScope(dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    loadInheritanceEDB(dataModel, feed)
    Loaded(engine, feed, compiled)
  }

  def loadFunction(code: String, options: ObjectOptions = ObjectOptions()): Loaded =
    loadFunction(compileObject(code, options))
}
