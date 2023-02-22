package inca.debugger

import inca.backend.ir.Datalog
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.Query
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import truechange.EditScript
import truechange.URI

// This class captures the state of the debugger
class DebuggerState(val bottomUpRuntime: DatalogRuntime) {

  val topDownDatabase: mutable.Map[Predicate, ValueTable] = mutable.Map.empty
  val activeQueries: mutable.Map[(Predicate, Adornment), ValueTable] = mutable.Map.empty

  def clear(): Unit = {
    topDownDatabase.clear()
    activeQueries.clear()
  }

  def readBottomUp(pred: Predicate, args: ValueTable): ValueTable = {
    val mainSpec = bottomUpRuntime.compiled.psystemModule.patterns.get(pred) match {
      case Some(spec) => spec()
      case None => return ValueTable.empty(Seq())
    }
    val mainMatcher = bottomUpRuntime.engine.getMatcher(mainSpec)
    val unboundCols = predicates(pred).map(_.name).diff(args.columns)
    val rows = args.entries.flatMap { row =>
      val inputMap = args.columns.zip(row.map(_.unwrap)).toMap ++ unboundCols.map(_ -> null)
      val input = Query.Match(mainSpec, inputMap, isMutable = false)
      val matches = mainMatcher.getAllMatches(input)
      matches.asScala.map { m =>
        m.toArray.map {
          case uri: URI => URIValue(uri)
          case v: Any => ScalaValue(v)
        }.toSeq
      }
    }
    ValueTable(mainMatcher.getParameterNames.asScala.toSeq, rows)
  }

  def countBottomUp(pred: Predicate, args: ValueTable): Int = {
    val mainSpec = bottomUpRuntime.compiled.psystemModule.patterns.get(pred) match {
      case Some(spec) => spec()
      case None => return 0
    }
    val mainMatcher = bottomUpRuntime.engine.getMatcher(mainSpec)
    val unboundCols = predicates(pred).map(_.name).diff(args.columns)
    args.entries.map { row =>
      val inputMap = args.columns.zip(row.map(_.unwrap)).toMap ++ unboundCols.map(_ -> null)
      val input = Query.Match(mainSpec, inputMap, isMutable = false)
      mainMatcher.countMatches(input)
    }.sum
  }

  def readBlacklistedBottomUp[A](pred: Predicate, args: ValueTable): ValueTable = {
    accessBlacklistedBottomUp(pred, args, readBottomUp)
  }

  private def accessBlacklistedBottomUp[A](
      pred: Predicate,
      args: ValueTable,
      f: (Predicate, ValueTable) => A
    ): A = {

    val blacklistMap = activeQueries.map { case ((pred, adornment), table) =>
      val blacklistName = BlacklistTransformation.extBlacklistName(pred, adornment)
      val tuples = table.entries.map { row =>
        val unwrapped = row.map(_.unwrap)
        Tuples.flatTupleOf(unwrapped: _*)
      }.toSet
      blacklistName -> tuples
    }.toMap
    // insert blacklist into runtime
    val insertBlacklist = DatabaseInput(EditScript(Seq()), blacklistMap, Map())
    val deleteBlacklist = DatabaseInput(EditScript(Seq()), Map(), blacklistMap)
    bottomUpRuntime.engine.delayUpdatePropagation { () =>
      bottomUpRuntime.db.processDatabaseInput(insertBlacklist)
    }
    val res = f(pred, args)
    // remove blacklist from runtime
    bottomUpRuntime.engine.delayUpdatePropagation { () =>
      bottomUpRuntime.db.processDatabaseInput(deleteBlacklist)
    }
    res
  }

  private def adorn(pred: Predicate, args: ValueTable): Adornment =
    predicates.get(pred) match {
      case Some(params) =>
        params.map { param =>
          args.isBound(param.name)
        }
      case None => throw IllegalDebugStateException(s"Predicate $pred is not defined")
    }

  private val predicates: Map[Predicate, Seq[Datalog.Param]] =
    bottomUpRuntime.compiled.ir.patternMap.map { case (pred, pat) =>
      pred -> pat.params
    }

  def insertTopDown(pred: Predicate, table: ValueTable): Unit = {
    topDownDatabase.get(pred) match {
      case Some(old) =>
        topDownDatabase += pred -> old.union(table)
      case None =>
        topDownDatabase += pred -> table
    }
  }

  def readTopDown(pred: Predicate, args: ValueTable): ValueTable = {
    topDownDatabase.get(pred) match {
      case Some(t) =>
        t.join(args)
      case None =>
        ValueTable.empty(predicates(pred).map(_.name))
    }
  }

  def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val adornment = adorn(pred, args)
    activeQueries.get(pred -> adornment) match {
      case Some(seen) =>
        val remaining = args.diff(seen)
        activeQueries += (pred -> adornment) -> seen.union(remaining)
        remaining
      case None =>
        activeQueries += (pred -> adornment) -> args
        args
    }
  }

  def popQuery(pred: Predicate, args: ValueTable): Unit = {
    val adornment = adorn(pred, args)
    activeQueries.get(pred -> adornment) match {
      case Some(seen) =>
        val remaining = seen.diff(args)
        activeQueries += (pred -> adornment) -> remaining
      case None =>
        throw IllegalDebugStateException(s"Remove query failed, query does not exist: $pred $args")
    }
  }

  def isStable(pred: Predicate, args: ValueTable, result: ValueTable): Boolean = {
    val topdown = readTopDown(pred, args)
    result.subset(topdown)
  }
}
