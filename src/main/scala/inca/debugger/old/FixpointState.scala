package inca.debugger.old

import inca.backend.ir.Datalog
import inca.debugger.table.ImmutableTable

import scala.collection.mutable
import scala.reflect.ClassTag

class FixpointState[V: ClassTag](
    patterns: Map[String, Datalog.Pattern]
  )(implicit valOrdering: Ordering[V],
    topAndBotFactory: () => (V, V)) {

  type Adornment = Seq[Boolean]

  private val derivedTuples: mutable.Map[String, ImmutableTable[V]] = mutable.Map()
  private val generalizedQueries: mutable.Map[(String, Adornment), ImmutableTable[V]] =
    mutable.Map()

  def addQuery(name: String, args: ImmutableTable[V]): Option[ImmutableTable[V]] = {
    val adorn = adornment(name, args)
    val nextQuery = generalizedQueries.get(name -> adorn) match {
      case Some(old) =>
        val unseen = args.diff(old)
        generalizedQueries(name -> adorn) = old.union(unseen)
        unseen
      case None =>
        generalizedQueries(name -> adorn) = args
        args
    }
    if (nextQuery.isEmpty) None
    else Some(nextQuery)
  }

  def addDerivedTuples(name: String, rel: ImmutableTable[V]): Unit = {
    derivedTuples.get(name) match {
      case Some(old) =>
        derivedTuples(name) = old.union(rel)
      case None =>
        derivedTuples(name) = rel
    }
  }

  def relation(name: String, args: ImmutableTable[V]): ImmutableTable[V] = {
    val table = relation(name)
    table.join(args)
    // args.join(table)
  }

  def relation(name: String): ImmutableTable[V] = {
    val params = patterns(name).params.map(_.name)
    val empty = ImmutableTable.empty[V](params)
    derivedTuples.getOrElse(name, empty)
  }

  private def adornment(name: String, args: ImmutableTable[V]): Adornment = {
    val pattern = patterns(name)
    pattern.params.map { p => args.isBound(p.name) }
  }

  override def toString: String = {
    s"""FixpointState(
      |  ${derivedTuples.map(x => s"${x._1} -> ${x._2}").mkString("\n  ")}
      |)
      |""".stripMargin
  }
}
