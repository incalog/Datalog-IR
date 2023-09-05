package inca.debugger

import inca.backend.analyze.DependencyGraph
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
  lazy val depGraph: DependencyGraph = new DependencyGraph(program)

  def readBottomUp(pred: Predicate, args: ValueTable): ValueTable = {
    throw new IllegalStateException("Reading bottom-up database not supported")
  }

  protected val topDownDatabase: mutable.Map[Predicate, ValueTable] = mutable.Map.empty
  protected val activeQueries: mutable.Map[(Predicate, Adornment), List[ValueTable]] =
    mutable.Map.empty
  protected lazy val predicates: Map[Predicate, Seq[Datalog.Param]] =
    program.patternMap.map { case (pred, pat) =>
      pred -> pat.params
    }

  def clear(): Unit = {
    topDownDatabase.clear()
    activeQueries.clear()
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

  def topActive(pred: Predicate, args: ValueTable): ValueTable = {
    val adornment = adorn(pred, args)
    activeQueries(pred -> adornment).head
  }

  def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val unseen = determineUnseen(pred, args)
    if (unseen.nonEmpty) {
      val adornment = adorn(pred, args)
      val newStack = args :: activeQueries.getOrElse(pred -> adornment, Nil)
      activeQueries += (pred -> adornment) -> newStack
    }
    unseen
  }

  protected def determineUnseen(pred: Predicate, args: ValueTable): ValueTable = {
    val adornment = adorn(pred, args)
    if (activeQueries.isDefinedAt(pred -> adornment)) {
      val seen = unionOfStack(pred, adornment)
      args.diff(seen)
    } else {
      args
    }
  }

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
    // result.subset(td)
    result.size <= td.size
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

  protected def isCyclic(pred: Predicate): Boolean = depGraph.cycles.exists(_.contains(pred))
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

trait AvoidNonProducingIterationDebuggerState extends BottomUpDebuggerState {
  protected val expectedFixpoint: mutable.Map[(Predicate, ValueTable), ValueTable] =
    mutable.Map.empty

  override def clear(): Unit = {
    super.clear()
    expectedFixpoint.clear()
  }

  override def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val unseen = determineUnseen(pred, args)
    expectedFixpoint += (pred -> unseen) -> readBottomUp(pred, unseen)
    super.pushQuery(pred, args)
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

// blacklist will be propagated and cleaned when pushing and popping a subquery
class AccumulatingDebuggerState(
    override val bottomUpRuntime: DatalogRuntime)
    extends BottomUpDebuggerState(bottomUpRuntime) {

  override def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val unseen = super.pushQuery(pred, args)
    if (unseen.nonEmpty && isCyclic(pred)) {
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
    if (originalArgs.nonEmpty && isCyclic(pred)) {
      val diff = originalArgs.diff(unionOfStack(pred, adorn(pred, args)))
      val blacklist = prepareBlacklist(pred, diff)
      val input = DatabaseInput(EditScript(Seq()), Map(), Map(blacklist))
      bottomUpRuntime.engine.delayUpdatePropagation { () =>
        bottomUpRuntime.db.processDatabaseInput(input)
      }
    }
    originalArgs
  }
}
// cases
// push, read
//   +insert, update
// push, pop, read
//   +insert, -insert, update
// push, read, pop, read
//   +insert, update and -insert(clear), +delete, update and -delete(clear)

class DelayingDebuggerState(override val bottomUpRuntime: DatalogRuntime)
    extends BottomUpDebuggerState(bottomUpRuntime) {

  private val collectedBlacklistInserts: mutable.Map[(Predicate, Adornment), ValueTable] =
    mutable.Map.empty
  private val collectedBlacklistDeletes: mutable.Map[(Predicate, Adornment), ValueTable] =
    mutable.Map.empty

  override def readBottomUp(pred: Predicate, args: ValueTable): ValueTable = {
    if (isCyclic(pred)) {
      val inserts = collectedBlacklistInserts.map { case ((pred, _), args) =>
        prepareBlacklist(pred, args)
      }.toMap
      val deletes = collectedBlacklistDeletes.map { case ((pred, _), args) =>
        prepareBlacklist(pred, args)
      }.toMap
      collectedBlacklistInserts.clear()
      collectedBlacklistDeletes.clear()
      val input = DatabaseInput(EditScript(Seq()), inserts, deletes)
      bottomUpRuntime.engine.delayUpdatePropagation { () =>
        bottomUpRuntime.db.processDatabaseInput(input)
      }
    }
    super.readBottomUp(pred, args)
  }

  override def pushQuery(pred: Predicate, args: ValueTable): ValueTable = {
    val unseen = super.pushQuery(pred, args)
    if (isCyclic(pred) && unseen.nonEmpty) {
      val adornment = adorn(pred, unseen)
      val empty = ValueTable.empty(unseen.columns)
      val newInsert = collectedBlacklistInserts.getOrElse(pred -> adornment, empty).union(unseen)
      val oldDelete = collectedBlacklistDeletes.getOrElse(pred -> adornment, empty)
      collectedBlacklistInserts += (pred -> adornment) -> newInsert.diff(oldDelete)
      collectedBlacklistDeletes += (pred, adornment) -> oldDelete.diff(newInsert)
    }
    unseen
  }

  override def popQuery(pred: Predicate, args: ValueTable): ValueTable = {
    if (isCyclic(pred)) {
      val adornment = adorn(pred, args)
      val empty = ValueTable.empty(args.columns)
      val newDelete = collectedBlacklistDeletes.getOrElse(pred -> adornment, empty).union(args)
      val oldInsert = collectedBlacklistInserts.getOrElse(pred -> adornment, empty)
      collectedBlacklistDeletes += (pred -> adornment) -> newDelete.diff(oldInsert)
      collectedBlacklistInserts += (pred, adornment) -> oldInsert.diff(newDelete)
    }
    super.popQuery(pred, args)
  }
}
