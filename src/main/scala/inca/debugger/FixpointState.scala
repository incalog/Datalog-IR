package inca.debugger

import inca.backend.ir.Datalog
import inca.debugger.table.Table

import scala.collection.mutable

class FixpointState[V](patterns: Map[String, Datalog.Pattern]) {
  type Adornment = Seq[Boolean]

  private val derivedTuples: mutable.Map[String, Table[V]] = mutable.Map()
  private val generalizedQueries: mutable.Map[(String, Adornment), Table[V]] = mutable.Map()

  def print(): Unit = {
    println(derivedTuples)
  }

  def addQuery(name: String, args: Table[V]): Option[Table[V]] = {
    val adorn = adornment(name, args)
    val nextQuery = generalizedQueries.get(name -> adorn) match {
      case Some(old) =>
        val unseen = args.diff(old)
        generalizedQueries(name -> adorn) = old.addRows(unseen)
        unseen
      case None =>
        generalizedQueries(name -> adorn) = args
        args
    }
    if (nextQuery.isEmpty) None
    else Some(nextQuery)
  }

  def addDerivedTuples(name: String, rel: Table[V]): Unit = {
    derivedTuples.get(name) match {
      case Some(old) =>
        derivedTuples(name) = old.addRows(rel)
      case None =>
        derivedTuples(name) = rel
    }
  }

  def relation(name: String, args: Table[V]): Table[V] =
    relation(name).join(args)

  def relation(name: String): Table[V] = {
    val params = patterns(name).params.map(_.name)
    val empty = Table.empty[V](params)
    derivedTuples.getOrElse(name, empty)
  }

  private def adornment(name: String, args: Table[V]): Adornment = {
    val pattern = patterns(name)
    pattern.params.map { p => args.isBound(p.name) }
  }
}
