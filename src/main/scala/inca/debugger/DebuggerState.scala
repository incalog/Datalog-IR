package inca.debugger

import inca.backend.ir.Datalog
import inca.runtime.db.DatabaseInput
import inca.runtime.DatalogRuntime
import inca.runtime.Query
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import truechange.EditScript
import truechange.URI

// This class captures the state of the debugger
trait DebuggerState {
  def bottomUpRuntime: DatalogRuntime
  lazy val program: Datalog.Module = bottomUpRuntime.compiled.ir

  def readBottomUp(pred: Predicate, args: ValueTable): ValueTable = {
    throw new IllegalStateException("Reading bottom-up database not supported")
  }

  protected val topDownDatabase: mutable.Map[Predicate, ValueTable] = mutable.Map.empty
  protected val activeQueries: mutable.Map[(Predicate, Adornment), List[ValueTable]] =
    mutable.Map.empty
//  protected val expectedFixpoint: mutable.Map[(Predicate, ValueTable), ValueTable] =
//    mutable.Map.empty
  protected lazy val predicates: Map[Predicate, Seq[Datalog.Param]] =
    program.patternMap.map { case (pred, pat) =>
      pred -> pat.params
    }

  def clear(): Unit = {
    topDownDatabase.clear()
    activeQueries.clear()
//    expectedFixpoint.clear()
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
        // println(s"read topdown $pred of ${t.columns} with args ${args.columns}")
        t.join(args)
      case None =>
        ValueTable.empty(predicates(pred).map(_.name))
    }
  }

  def topActive(pred: Predicate, args: ValueTable): ValueTable = {
    val adornment = adorn(pred, args)
    activeQueries(pred -> adornment).head
  }

  def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val adornment = adorn(pred, args)
    activeQueries.get(pred -> adornment) match {
      case Some(stack) =>
        val seen = unionOfStack(pred, adornment)
        val remaining = args.diff(seen)
        if (remaining.nonEmpty) {
          activeQueries += (pred -> adornment) -> (args :: stack)
        }
        remaining
      case None =>
        activeQueries += (pred -> adornment) -> List(args)
        args
    }
  }

//  def storeExpectedFixpoint(pred: Predicate, args: ValueTable): Unit = {
  // expectedFixpoint += (pred -> args) -> readBottomUp(pred, args)
//  }
//
//  def clearExpectedFixpoint(pred: Predicate, args: ValueTable): Unit = {
  // expectedFixpoint.remove(pred -> args)
//  }

  def popQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val adornment = adorn(pred, args)
    activeQueries.get(pred -> adornment) match {
      case Some(stack) =>
        activeQueries += (pred -> adornment) -> stack.tail
        stack.head
      case None =>
        throw IllegalDebugStateException(s"Remove query failed, query does not exist: $pred $args")
    }
  }

  def isStable(pred: Predicate, args: ValueTable, result: ValueTable): Boolean = {
    val td = readTopDown(pred, args)
    result.subset(td)
//    expectedFixpoint.get(pred -> args) match {
//      case Some(expected) =>
//        expected.size <= result.size
//      case None =>
//        throw new IllegalStateException("")
//    }
  }

  protected def unionOfStack(pred: Predicate, adornment: Adornment): ValueTable = {
    val allColumns = predicates(pred).map(_.name)
    val columns = adornment.zipWithIndex.flatMap { case (a, i) =>
      if (a) Some(allColumns(i))
      else None
    }
    activeQueries.get(pred -> adornment) match {
      case Some(stack) =>
        stack.foldLeft(ValueTable.empty(columns)) { case (res, v) => res.union(v) }
      case None => ValueTable.empty(columns)
    }
  }

  protected def adorn(pred: Predicate, args: ValueTable): Adornment =
    predicates.get(pred) match {
      case Some(params) =>
        params.map { param =>
          args.isBound(param.name)
        }
      case None => throw IllegalDebugStateException(s"Predicate $pred is not defined")
    }

  protected def prepareBlacklist(pred: Predicate, args: ValueTable): (String, Set[Tuple]) = {
    val adornment = adorn(pred, args)
    val blacklistName = BlacklistTransformation.extBlacklistName(pred, adornment)
    val tuples = args.entries.map { row =>
      val unwrapped = row.map(_.unwrap)
      Tuples.flatTupleOf(unwrapped: _*)
    }.toSet
    blacklistName -> tuples
  }

}

class BottomUpDebuggerState(val bottomUpRuntime: DatalogRuntime) extends DebuggerState {
  override def readBottomUp(pred: Predicate, args: ValueTable): ValueTable = {
    val spec = bottomUpRuntime.compiled.psystemModule.patterns.get(pred) match {
      case Some(spec) => spec()
      case None => return ValueTable.empty(Seq())
    }
    val matcher = bottomUpRuntime.engine.getMatcher(spec)
    val unboundCols = predicates(pred).map(_.name).diff(args.columns)
    val rows = args.entries.flatMap { row =>
      val inputMap = args.columns.zip(row.map(_.unwrap)).toMap ++ unboundCols.map(_ -> null)
      val input = Query.Match(spec, inputMap, isMutable = false)
      val matches = matcher.getAllMatches(input)
      matches.asScala.map { m =>
        m.toArray.map {
          case uri: URI => URIValue(uri)
          case v: Any => ScalaValue(v)
        }.toSeq
      }
    }
    ValueTable(matcher.getParameterNames.asScala.toSeq, rows)
  }
}

// TODO
trait AvoidNonProducingIterationDebuggerState extends BottomUpDebuggerState {
  protected val expectedFixpoint: mutable.Map[(Predicate, ValueTable), ValueTable] =
    mutable.Map.empty

  override def clear(): Unit = {
    super.clear()
    expectedFixpoint.clear()
  }

  override def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val unseen = super.pushQuery(pred, args)
    expectedFixpoint += (pred -> unseen) -> readBottomUp(pred, unseen)
    unseen
  }

  override def popQuery(pred: Predicate, args: ValueTable): ValueTable = {
    expectedFixpoint.remove(pred -> args)
    super.popQuery(pred, args)
  }

  override def isStable(pred: Predicate, args: ValueTable, result: ValueTable): Boolean = {
    expectedFixpoint.get(pred -> args) match {
      case Some(expected) =>
        expected.size <= result.size
      case None =>
        throw new IllegalStateException("")
    }
  }
}

// blacklist will be filled before reading bottom up and then will be reset immediately
class ResettingDebuggerState(override val bottomUpRuntime: DatalogRuntime)
    extends BottomUpDebuggerState(bottomUpRuntime) {
  override def readBottomUp(pred: Predicate, args: ValueTable): ValueTable = {
    val blacklistMap = activeQueries.map { case ((pred, adornment), _) =>
      val seen = unionOfStack(pred, adornment)
      prepareBlacklist(pred, seen)
    }.toMap
    // insert blacklist into runtime
    val insertBlacklist = DatabaseInput(EditScript(Seq()), blacklistMap, Map())
    val deleteBlacklist = DatabaseInput(EditScript(Seq()), Map(), blacklistMap)
    bottomUpRuntime.engine.delayUpdatePropagation { () =>
      bottomUpRuntime.db.processDatabaseInput(insertBlacklist)
    }
    val res = super.readBottomUp(pred, args)
    // remove blacklist from runtime
    bottomUpRuntime.engine.delayUpdatePropagation { () =>
      bottomUpRuntime.db.processDatabaseInput(deleteBlacklist)
    }
    res
  }
}

class AccumulatingDebuggerState(override val bottomUpRuntime: DatalogRuntime)
    extends BottomUpDebuggerState(bottomUpRuntime) {

  override def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val unseen = super.pushQuery(pred, args)
    if (unseen.nonEmpty) {
      val blacklist = prepareBlacklist(pred, unseen)
      val input = DatabaseInput(EditScript(Seq()), Map(blacklist), Map())
      bottomUpRuntime.engine.delayUpdatePropagation { () =>
        bottomUpRuntime.db.processDatabaseInput(input)
      }
    }
    unseen
  }

  override def popQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val originalArgs = super.popQuery(pred, args)
    if (originalArgs.nonEmpty) {
      val blacklist = prepareBlacklist(pred, originalArgs)
      val input = DatabaseInput(EditScript(Seq()), Map(), Map(blacklist))
      bottomUpRuntime.engine.delayUpdatePropagation { () =>
        bottomUpRuntime.db.processDatabaseInput(input)
      }
    }
    originalArgs
  }
}
