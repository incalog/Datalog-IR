package inca.debugger

import inca.debugger.table.indexing.IndexCover
import inca.debugger.table.ImmutableTable

package object redesign_new {
  type Predicate = String
  type Adornment = Seq[Boolean]
  type ValueTable = ImmutableTable[Value]
  object ValueTable {
    def unit(): ValueTable = ImmutableTable.unit()
    def empty(cols: Seq[String]): ValueTable = ImmutableTable.empty(cols)
    def apply(
        cols: Seq[String],
        entries: Seq[Seq[Value]],
        indexCovers: Set[IndexCover] = Set()
      ): ValueTable =
      ImmutableTable(cols, entries, indexCovers)
  }
}
