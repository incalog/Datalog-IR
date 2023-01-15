package inca.debugger.redesign_new

import inca.backend.ir.Datalog
import inca.debugger.IllegalDebugStateException
import inca.debugger.ScalaValue
import inca.debugger.URIValue
import inca.debugger.Value
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.Query
import inca.util.TimeTracker
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import truechange.EditScript
import truechange.URI

// This class captures the state of the debugger
class DebuggerState(val bottomUpRuntime: DatalogRuntime) {

  // we dont use a table but a bag instead
  type Bag = Map[Seq[Value], Int]
  val blacklist: mutable.Map[(Predicate, Adornment), Bag] = mutable.Map.empty
  val topDownResults: mutable.Map[Predicate, ValueTable] = mutable.Map.empty
  val seenQueries: mutable.Map[(Predicate, Adornment), ValueTable] = mutable.Map.empty
  val fixpointSize: mutable.Map[(Predicate, ValueTable, Int), Int] = mutable.Map.empty

  def clear(): Unit = {
    blacklist.clear()
    topDownResults.clear()
    seenQueries.clear()
    fixpointSize.clear()
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
    TimeTracker.begin()
    val mainMatcher = bottomUpRuntime.engine.getMatcher(mainSpec)
    TimeTracker.stop()
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

    val blacklistMap = blacklist.map { case ((pred, adornment), bag) =>
      val blacklistName = BlacklistTransformation.extBlacklistName(pred, adornment)
      val tuples = bag.filter(_._2 > 0).map { case (row, _) =>
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

  def storeExpectedFixpointSize(pred: Predicate, args: ValueTable, stackHeight: Int): Unit = {
    val bottomUpSize = readBlacklistedBottomUp(pred, args).size
    fixpointSize += (pred, args, stackHeight) -> bottomUpSize
  }

  def insertBlacklist(pred: Predicate, args: ValueTable): Unit = {
    val adornment = adorn(pred, args)
    blacklist.get(pred -> adornment) match {
      case Some(old) =>
        var newBag = old
        args.entries.foreach { row =>
          val oldValue = old.getOrElse(row, 0)
          newBag = newBag + (row -> (oldValue + 1))
        }
        blacklist += (pred -> adornment) -> newBag
      case None =>
        blacklist += (pred -> adornment) -> args.entries.map(_ -> 1).toMap
    }
  }

  def deleteBlacklist(pred: Predicate, args: ValueTable): Unit = {
    val adornment = adorn(pred, args)
    blacklist.get(pred -> adornment) match {
      case Some(old) =>
        var newBag = old
        args.entries.foreach { row =>
          val oldValue = if (old.getOrElse(row, 0) == 0) 1 else old(row)
          newBag = newBag + (row -> (oldValue - 1))
        }
        blacklist += (pred -> adornment) -> newBag
      case None =>
        blacklist += (pred -> adornment) -> args.entries.map(_ -> 0).toMap
    }
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
    topDownResults.get(pred) match {
      case Some(old) =>
        topDownResults += pred -> old.union(table)
      case None =>
        topDownResults += pred -> table
    }
  }

  def readTopDown(pred: Predicate, args: ValueTable): ValueTable = {
    topDownResults.get(pred) match {
      case Some(t) =>
        t.join(args)
      case None =>
        ValueTable.empty(predicates(pred).map(_.name))
    }
  }

  def addNewQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val adornment = adorn(pred, args)
    // We don't need to reset this at any point
    // If we have already seen this all of this query already and see it again, we will already have derived the fixpoint
    seenQueries.get(pred -> adornment) match {
      case Some(seen) =>
        val remaining = args.diff(seen)
        seenQueries += (pred -> adornment) -> seen.union(remaining)
        remaining
      case None =>
        seenQueries += (pred -> adornment) -> args
        args
    }
  }

  def isStable(pred: Predicate, args: ValueTable, result: ValueTable, stackHeight: Int): Boolean = {
    val topDown = readTopDown(pred, args)
    val current = topDown.union(result)
    val currentSize = current.size
    val expectedSize = fixpointSize((pred, args, stackHeight))
    currentSize >= expectedSize
  }
}
