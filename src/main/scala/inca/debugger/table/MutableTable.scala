package inca.debugger.table

trait MutableTable[V] {
  type Tuple = Seq[V]
  type NamedTuple = Map[String, V]

  def columns: Seq[String]
  def entries: Seq[Tuple]
  def contains(t: NamedTuple): Boolean
  def contains(t: Tuple): Boolean
  def join(other: MutableTable[V]): Unit
  def insert(t: Tuple): Unit
}
