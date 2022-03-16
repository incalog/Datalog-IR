package inca.debugger.table

/**
 * Edge(x,y), z := y, IsConnected(x,y)
 *
 *
 * type Table = (Columns, ColumnsIndices, Data)
 * type ColumnsIndices = Map[String, Int]
 * type Data = Map[ArraySeq[V], Int]
 */

trait Table[V] {
  def columns: Seq[String]
  def rows: Iterable[Seq[V]]

  def isEmpty: Boolean
  def isBound(col: String): Boolean

  def bind(col: String, v: V): Table[V]
  def join(other: Table[V]) : Table[V]
  def addRow(row: Seq[V]): Table[V]
  def addRows(rows: Table[V]): Table[V]
  def diff(other: Table[V]): Table[V]
  def numRows: Int

  def project(cols: Seq[String]): Table[V]
  def project(subst: Map[String, String]): Table[V]
  def renameColumns(subst: Map[String, String]): Table[V]
  def rearrangeColumns(cols: Seq[String]): Table[V]

  def columnIndex(col: String): Int

  def filter(f: Seq[V] => Boolean): Table[V]
  def flatMap(f: Seq[V] => Seq[Seq[V]]): Table[V]
  def map(f: Seq[V] => Seq[V]): Table[V]
  def expand(newcol: String, f: Seq[V] => V): Table[V]
  def expand(newcols: Seq[String], f: Seq[V] => Seq[V]): Table[V]

  def contains(colValPairs: Seq[(String, V)]): Boolean

  def bindingsToString(f: V => String): String = {
    val rowStrings = rows.map { row =>
      val sb = new StringBuilder
      sb += '['
      columns.foreach { col =>
        val ix = columnIndex(col)
        val v = row(ix)
        if (v != null) {
          sb ++= col
          sb += '='
          sb ++= f(v)
          sb ++= ", "
        }
      }
      if (sb.length() > 2) {
        sb.deleteCharAt(sb.length() - 1)
        sb.deleteCharAt(sb.length() - 1)
      }
      sb += ']'
      sb.toString()
    }
    rowStrings.size match {
      case 0 => "nil"
      case 1 => rowStrings.head
      case _ => rowStrings.mkString("{", ", ", "}")
    }
  }
}

object Table {
  def empty[V]: Table[V] = SimpleTable(Vector(), Vector())
  def empty[V](columns: Seq[String]): Table[V] = SimpleTable(columns.toVector, Vector())
  def unit[V]: Table[V] = SimpleTable(Vector(), Vector(Vector()))
  def apply[V](columns: Seq[String], rows: Iterable[Iterable[V]]): Table[V] = {
    if (rows.iterator.exists(_.size != columns.size))
      throw new IllegalArgumentException(s"Ill-formed table with columns $columns and rows $rows")
    SimpleTable(columns.toVector, rows.iterator.map(_.iterator.toVector).toVector)
  }
  def apply[V](map: Map[String, V]): Table[V] = {
    val (colums, row) = map.toVector.unzip
    SimpleTable(colums, Vector(row))
  }
}
