package inca.debugger

/**
 * Edge(x,y), z := y, IsConnected(x,y)
 *
 *
 * type Table = (Columns, ColumnsIndices, Data)
 * type ColumnsIndices = Map[String, Int]
 * type Data = Map[ArraySeq[V], Int]
 */

// first implementation, we do not consider efficiency
case class Table(columns: Vector[String], data: Vector[Vector[Value]]) {

  def isBound(col: String): Boolean = columns.contains(col)

  def renameColumns(columnsSubst: Map[String, String]): Table = {
    Table(columns.map(columnsSubst.apply), data)
  }

  def addRow(row: Vector[Value]): Table = {
    val newData =
      if (data.contains(row))
        data
      else
        data :+ row
    Table(columns, newData)
  }

  def addRows(rows: Vector[Vector[Value]]): Table = {
    Table(columns, (data ++ rows).distinct)
  }

  def bind(column: String, v: Value): Table = {
    val colIdx = columns.indexOf(column)
    if (colIdx > -1) {
      val newData = data.filter { row =>
        row(colIdx) == v
      }
      Table(columns, newData)
    } else {
      val newData = data.map { row =>
        row :+ v
      }
      Table(columns :+ column, newData)
    }
  }

  def project(col: String): Vector[Value] = {
    data.map { row =>
      val colIdx = columns.indexOf(col)
      if (colIdx > -1) row(colIdx)
      else throw new IllegalArgumentException(s"Cannot project column $col out of table with columns ${columns.mkString(", ")}")
    }
  }

  def project(cols: Vector[String]): Table = {
    val newColumns = cols.filter(columns.contains)
    val newData = data.map { row =>
      newColumns.flatMap { col =>
        val colIdx = columns.indexOf(col)
        if (colIdx > -1) {
          if (colIdx >= row.size) Vector()
          else Vector(row(colIdx))
        } else Vector() // throw new IllegalArgumentException(s"Cannot project column $col out of table with columns ${columns.mkString(", ")}")
      }
    }
    Table(newColumns, newData)
  }

  def join(other: Table): Table = {
    val otherCols = other.columns.diff(columns)
    val otherColsIdx = otherCols.map(other.columns.indexOf)
    val newColumns = columns ++ otherCols
    val sameCols = columns.filter(other.columns.contains)
    val sameColIdxs = sameCols.map(columns.indexOf)
    val otherSameColIdxs = sameCols.map(other.columns.indexOf)
    val newData = data.flatMap { row =>
      val sameColVals = sameColIdxs.map(row.apply)
      other.data.filter { otherRow =>
        val otherSameColVals = otherSameColIdxs.map(otherRow.apply)
        sameColVals == otherSameColVals
      }.map { otherRow =>
        row ++ otherColsIdx.map(otherRow.apply)
      }
    }
    Table(newColumns, newData)
  }
}
object Table {
  def empty: Table = Table(Vector(), Vector())
}
