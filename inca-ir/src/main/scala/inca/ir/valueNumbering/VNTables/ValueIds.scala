package inca.ir.valueNumbering.VNTables

import inca.ir
import inca.util.Tabulator

import scala.collection.mutable


type ValueNumber = Int

/** provides mapping from [[T]] to [[ValueNumber]]
 *
 * @tparam T type of construct that receives a value number (e.g. Term)
 * @throws IllegalStateException if number of different values exceeds range of [[Int]]
 */
class ValueIds[T]{
  private val ids: mutable.Map[T,ValueNumber] = mutable.Map()

  private var currentId: ValueNumber = Int.MinValue
  private def nextId(): ValueNumber = {
    if (currentId == Int.MaxValue){
      throw IllegalStateException("Value Numbering: Too many CongruenceClasses -> Overflow in ValueIds")
    }
    currentId += 1
    currentId
  }

  def getValNumOf(t: T): ValueNumber = ids.getOrElse(t,{
    ids.update(t,nextId())
    ids(t)
  })
  def apply(t: T): ValueNumber = getValNumOf(t)

  def contains(t: T): Boolean = ids.contains(t)

  def update(t: T, valueId: ValueNumber): Unit = ids.update(t, valueId)

  def clear(): Unit = {
    currentId = Int.MinValue
    ids.clear()
  }

  override def toString: String = "Value Numbers: \t\t\t" + ids.mkString(";  ")

  // just for printing and debugging (constructing congrClasses like this every time is too computationally complex)
  private def congrClasses: Map[ValueNumber, Seq[T]] = ids.groupBy(_._2).map((id, m) => id -> m.keys.toSeq)

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

    Tabulator.format("Entries for Value Numbers: ", header, entries)
  }

  def printResults(): Unit = {
    println("VN Results: ")
    println(congrClassesStr)
    println("")
  }

  def getAllWithVn(vn: ValueNumber): Seq[T] = {
    ids.keys.filter(t => ids(t) == vn).toSeq
  }
  
  def updateAll(fromVn: ValueNumber, toVn: ValueNumber): Unit = {
    getAllWithVn(fromVn).foreach(update(_,toVn))
  }
  
  
}
