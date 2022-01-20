package inca.debugger.table

import inca.debugger.Value

/**
 * Edge(x,y), z := y, IsConnected(x,y)
 *
 *
 * type Table = (Columns, ColumnsIndices, Data)
 * type ColumnsIndices = Map[String, Int]
 * type Data = Map[ArraySeq[V], Int]
 */

trait Table {
  def columns: Seq[String]
  def rows: Iterable[Seq[Value]]

  def isEmpty: Boolean
  def isBound(col: String): Boolean

  def bind(col: String, v: Value): Table
  def bind(col: String, vs: Seq[Value]): Table
  def join(other: Table) : Table
  def addRow(row: Seq[Value]): Table
  def addRows(rows: Table): Table
  def addColumn(col: String): Table

  def project(cols: Seq[String]): Table
  def renameColumns(subst: Map[String, String]): Table
  def rearrangeColumns(cols: Seq[String]): Table

  def columnIndex(col: String): Int

  def filter(f: Seq[Value] => Boolean): Table
  def flatMap(f: Seq[Value] => Seq[Seq[Value]]): Table
  def map(f: Seq[Value] => Seq[Value]): Table
  def expand(newcol: String, f: Seq[Value] => Value): Table
  def expand(newcols: Seq[String], f: Seq[Value] => Seq[Value]): Table

  def contains(colValPairs: Seq[(String, Value)]): Boolean

}

object Table {
  def empty: Table = SimpleTable(Vector(), Vector())
  def empty(columns: Seq[String]): Table = SimpleTable(columns.toVector, Vector())
  def unit: Table = SimpleTable(Vector(), Vector(Vector()))
  def apply(columns: Seq[String], rows: Seq[Seq[Value]]): Table =
    SimpleTable(columns.toVector, rows.map(_.toVector).toVector)
}
