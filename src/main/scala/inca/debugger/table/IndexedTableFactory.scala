package inca.debugger.table

import inca.debugger.table.indexing.IndexCover
import scala.reflect.ClassTag

class IndexedTableFactory[V: ClassTag](
    parametersOfRelationTable: Map[String, Seq[String]]
  )(implicit val valOrdering: Ordering[V],
    topAndBotFactory: () => (V, V)) {

  // Parameter name is being used to construct an appropriate index such that the table can be used on the left-hand side of a join operation
  def apply(
      columns: Seq[String],
      entries: Seq[Seq[V]],
      name: String
    ): ImmutableTable[V] = {
    val params = parametersOfRelationTable(name)
    apply(columns, entries, params)
  }

  // Parameter name is being used to construct an appropriate index such that the table can be used on the left-hand side of a join operation
  def apply(
      columns: Seq[String],
      entries: Seq[Seq[V]],
      indexConformingColumns: Seq[String]
    ): ImmutableTable[V] = {
    val indexCovers = constructIndexCovers(indexConformingColumns, columns)
    ImmutableTable(columns, entries, indexCovers = indexCovers)
  }

  def apply(
      columns: Seq[String],
      entries: Seq[Seq[V]],
      otherTable: ImmutableTable[V]
    ): ImmutableTable[V] = {
    val indexCovers = constructIndexCovers(otherTable.columns, columns)
    ImmutableTable(columns, entries, indexCovers = indexCovers)
  }

  def apply(
      columns: Seq[String],
      entries: Seq[Seq[V]],
      indexCovers: Set[IndexCover]
    ): ImmutableTable[V] = {
    ImmutableTable(columns, entries, indexCovers = indexCovers)
  }

  def constructIndexCovers(lhs: Seq[String], rhs: Seq[String]): Set[IndexCover] = {
    val sameInRhs = rhs.filter(lhs.contains)
    val indexCover = IndexCover(sameInRhs)
    Set(indexCover)
  }

  def constructIndexCovers(lhs: ImmutableTable[V], rhs: Seq[String]): Set[IndexCover] =
    constructIndexCovers(lhs.columns, rhs)
}
