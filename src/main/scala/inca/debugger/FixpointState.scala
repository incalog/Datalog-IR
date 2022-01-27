package inca.debugger

import inca.debugger.table.Table

import scala.collection.mutable


/*
 *
 * p(x,y) :- p(x,z),e(z,y).
 * p(x,y) :- e(x,y).
 *
 * p(1,5) -> p(1)=z0 -> p(1)=z1
 * = p(1,2)
 * p(1,5) -> p(1)=2 -> e(2)=3
 * = p(1,3)
 * p(1,5) -> p(1)=3 -> e(3)=4
 * = p(1,4)
 * p(1,5) -> p(1)=4 -> e(4)=5
 * = p(1,5)
 *
 *
 */



class FixpointState {
  private val derived: mutable.Map[(String, Table[Value]), Table[Value]] = mutable.Map()

  def contains(name: String, args: Table[Value]): Boolean = derived.contains(name -> args)

  def add(name: String, args: Table[Value], rel: Table[Value]): Unit = {
      derived.get(name -> args) match {
        case Some(old) =>
            derived += (name -> args) -> old.addRows(rel)
        case None =>
          derived += (name -> args) -> rel
      }
  }

  def relation(name: String, args: Table[Value]): Option[Table[Value]] =
    derived.get(name -> args)

  def relation(name: String): Table[Value] = {
    val tables = derived.collect {
      case ((relName, _), rel) if name == relName=>
        rel
    }.toSeq
    var res = tables.head
    tables.tail.foreach { t =>
      res = res.addRows(t)
    }
    res
  }
}
