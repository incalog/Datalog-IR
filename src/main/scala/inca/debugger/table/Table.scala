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

// first implementation, we do not consider efficiency

trait Table {
  def columns: Seq[String]
  def data: Seq[Seq[Value]]

  def isEmpty: Boolean
  def isBound(col: String): Boolean

  def bind(col: String, v: Value): Table
  def join(other: Table) : Table
  def addRow(row: Seq[Value]): Table
  def addRows(rows: Table): Table

  // def project(col: String): Table

  def project(cols: Seq[String]): Table
  def renameColumns(subst: Map[String, String]): Table
  def rearrangeColumns(cols: Seq[String]): Table
}

object Table {
  def empty: Table = SimpleTable(Vector(), Vector())
  def apply(columns: Seq[String], rows: Seq[Seq[Value]]): Table =
    SimpleTable(columns.toVector, rows.map(_.toVector).toVector)
}
