package inca.debugger.redesign_old

import inca.backend.ir.Datalog
import inca.debugger.table.ImmutableTable
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
  val topDownResults: mutable.Map[Predicate, ImmutableTable[Value]] = mutable.Map.empty
  val seenQueries: mutable.Map[(Predicate, Adornment), ImmutableTable[Value]] = mutable.Map.empty
//  val fixpointSize: mutable.Map[(Predicate, ImmutableTable[Value]), Int] =
//    mutable.Map.empty

  def clear(): Unit = {
    blacklist.clear()
    topDownResults.clear()
    seenQueries.clear()
//    fixpointSize.clear()
  }

  def readBottomUp(p: Predicate, args: ImmutableTable[Value]): ImmutableTable[Value] = {
    val mainSpec = bottomUpRuntime.compiled.psystemModule.patterns.get(p) match {
      case Some(spec) => spec()
      case None => return ImmutableTable.empty[Value](Seq())
    }
    val mainMatcher = bottomUpRuntime.engine.getMatcher(mainSpec)
    val unboundCols = predicates(p).map(_.name).diff(args.columns)
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
    ImmutableTable(mainMatcher.getParameterNames.asScala.toSeq, rows)
  }

  def countBottomUp(p: Predicate, args: ImmutableTable[Value]): Int = {
    val mainSpec = bottomUpRuntime.compiled.psystemModule.patterns.get(p) match {
      case Some(spec) => spec()
      case None => return 0
    }
    TimeTracker.begin()
    val mainMatcher = bottomUpRuntime.engine.getMatcher(mainSpec)
    TimeTracker.stop()
    val unboundCols = predicates(p).map(_.name).diff(args.columns)
    args.entries.map { row =>
      val inputMap = args.columns.zip(row.map(_.unwrap)).toMap ++ unboundCols.map(_ -> null)
      val input = Query.Match(mainSpec, inputMap, isMutable = false)
      mainMatcher.countMatches(input)
    }.sum
  }

  def readBlacklistedBottomUp[A](
      p: Predicate,
      args: ImmutableTable[Value]
    ): ImmutableTable[Value] = {
    accessBlacklistedBottomUp(p, args, readBottomUp)
  }

  private def accessBlacklistedBottomUp[A](
      p: Predicate,
      args: ImmutableTable[Value],
      f: (Predicate, ImmutableTable[Value]) => A
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
    val res = f(p, args)
    // remove blacklist from runtime
    bottomUpRuntime.engine.delayUpdatePropagation { () =>
      bottomUpRuntime.db.processDatabaseInput(deleteBlacklist)
    }
    res
  }

//  def storeExpectedFixpointSize(p: Predicate, args: ImmutableTable[Value]): Unit = {
//    fixpointSize += (p, args) -> accessBlacklistedBottomUp(p, args, countBottomUp)
//  }

  def insertBlacklist(p: Predicate, args: ImmutableTable[Value]): Unit = {
    val adornment = adorn(p, args)
    blacklist.get(p -> adornment) match {
      case Some(old) =>
        var newBag = old
        args.entries.foreach { row =>
          val oldValue = old.getOrElse(row, 0)
          newBag = newBag + (row -> (oldValue + 1))
        }
        blacklist += (p -> adornment) -> newBag
      case None =>
        blacklist += (p -> adornment) -> args.entries.map(_ -> 1).toMap
    }
  }

  def deleteBlacklist(p: Predicate, args: ImmutableTable[Value]): Unit = {
    val adornment = adorn(p, args)
    blacklist.get(p -> adornment) match {
      case Some(old) =>
        var newBag = old
        args.entries.foreach { row =>
          val oldValue = if (old.getOrElse(row, 0) == 0) 1 else old(row)
          newBag = newBag + (row -> (oldValue - 1))
        }
        blacklist += (p -> adornment) -> newBag
      case None =>
        blacklist += (p -> adornment) -> args.entries.map(_ -> 0).toMap
    }
  }

  private def adorn(p: Predicate, args: ImmutableTable[Value]): Adornment =
    predicates.get(p) match {
      case Some(params) =>
        params.map { param =>
          args.isBound(param.name)
        }
      case None => throw IllegalDebugStateException(s"Predicate $p is not defined")
    }

  private val predicates: Map[Predicate, Seq[Datalog.Param]] =
    bottomUpRuntime.compiled.ir.patternMap.map { case (p, pat) =>
      p -> pat.params
    }

  def insertTopDown(p: Predicate, table: ImmutableTable[Value]): Unit = {
    topDownResults.get(p) match {
      case Some(old) =>
        topDownResults += p -> old.union(table)
      case None =>
        topDownResults += p -> table
    }
  }

  def readTopDown(p: Predicate, args: ImmutableTable[Value]): ImmutableTable[Value] = {
    topDownResults.get(p) match {
      case Some(t) =>
        t.join(args)
      case None =>
        ImmutableTable.empty(predicates(p).map(_.name))
    }
  }

  def filterSeenQueries(p: Predicate, args: ImmutableTable[Value]): ImmutableTable[Value] = {
    val adornment = adorn(p, args)
    // We don't need to reset this at any point
    // If we have already seen this all of this query already and see it again, we will already have derived the fixpoint
    seenQueries.get(p -> adornment) match {
      case Some(seen) =>
        val remaining = args.diff(seen)
        seenQueries += (p -> adornment) -> seen.union(remaining)
        remaining
      case None =>
        seenQueries += (p -> adornment) -> args
        args
    }
  }

  def addSeenQuery(p: Predicate, args: ImmutableTable[Value]): Unit = {
    val adornment = adorn(p, args)
    val old = seenQueries.getOrElse(p -> adornment, ImmutableTable.empty[Value](args.columns))
    seenQueries += (p -> adornment) -> old.union(args)
  }

  def isUnstable(p: Predicate, args: ImmutableTable[Value]): Boolean = {
    val topDown = readTopDown(p, args)
    val topDownSize = topDown.size
    val bottomUpSize = accessBlacklistedBottomUp(p, args, countBottomUp)
    topDownSize < bottomUpSize
  }
}
