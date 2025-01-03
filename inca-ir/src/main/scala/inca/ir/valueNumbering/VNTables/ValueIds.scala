package inca.ir.valueNumbering.VNTables

import inca.ir
import inca.util.Tabulator

import scala.collection.mutable


type ValueId = Int

/** for value numbering; provides mapping from [[T]] to [[ValueId]]
 *
 * @tparam T type of construct that receives a value number (e.g. Term)
 * @throws IllegalStateException if number of different values exceeds range of [[Int]]
 */
class ValueIds[T]{
  private val ids: mutable.Map[T,ValueId] = mutable.Map()

  private var currentId: ValueId = Int.MinValue
  private def nextId(): ValueId = {
    if (currentId == Int.MaxValue){
      throw IllegalStateException("Value Numbering: Too many CongruenceClasses -> Overflow in ValueIds")
    }
    currentId += 1
    currentId
  }

  def getIdOf(t: T): ValueId = ids.getOrElse(t,{
    ids.update(t,nextId())
    ids(t)
  })
  def apply(t: T): ValueId = getIdOf(t)

  def contains(t: T): Boolean = ids.contains(t)

  def update(t: T, valueId: ValueId): Unit = ids.update(t, valueId)

  def clear(): Unit = {
    currentId = Int.MinValue
    ids.clear()
  }

  override def toString: String = "IDs: \t\t\t" + ids.mkString(";  ")

  // just for printing and debugging (constructing congrClasses like this every time is too computationally complex)
  private def congrClasses: Map[ValueId, Seq[T]] = ids.groupBy(_._2).map((id, m) => id -> m.keys.toSeq)

  private def congrClassesStr: String = {
    val c = congrClasses
    val size = (c.values ++ Seq(c.keys)).map(_.size).max
    val entries = c.values.toSeq.map{e =>
      if (e.size < size) {
        e.appendedAll((0 until (size - e.size)).map(_ => ""))
      }
      else e
    }.transpose
    val header = c.keys.toSeq.map(_.toString)

    Tabulator.format("Terms for Ids: ", header, entries)
  }

  def printResults(): Unit = {
    println("VN Results: ")
    println(congrClassesStr)
    println("")
  }

  def getAllWithId(id: ValueId): Seq[T] = {
    ids.keys.filter(t => ids(t) == id).toSeq
  }
  
  def updateAll(fromId: ValueId, toId: ValueId): Unit = {
    getAllWithId(fromId).foreach(update(_,toId))
  }
  
  
}
