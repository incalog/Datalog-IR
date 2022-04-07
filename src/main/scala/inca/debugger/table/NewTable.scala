package inca.debugger.table

import inca.debugger.table.indexing.IndexCover

trait NewTable[V] {
  type Tuple = Seq[V]
  type NamedTuple = Seq[(String, V)]
  def columns: Seq[String]
  def isEmpty: Boolean
  def size: Int
  def isBound(column: String): Boolean
  def columnIndex(column: String): Int
  def entries(indexCover: IndexCover): Seq[Tuple]
  def entries(namedTuple: NamedTuple): Seq[Tuple]
  def entries: Seq[Tuple]
  def contains(t: Tuple, indexCover: IndexCover): Boolean
  def contains(t: NamedTuple): Boolean
}
