package inca.debugger.table

import inca.debugger.Value

case class SimpleTable(columns: Vector[String], data: Vector[Vector[Value]]) extends Table {

  override def isEmpty: Boolean = data.isEmpty
  override def isBound(col: String): Boolean = columns.contains(col)

  override def renameColumns(columnsSubst: Map[String, String]): Table = {
    Table(columns.map(columnsSubst.apply), data)
  }

  override def addRow(row: Seq[Value]): Table = {
    val newData =
      if (data.contains(row))
        data
      else
        data :+ row
    Table(columns, newData)
  }

  // override def addRows(rows: Seq[Seq[Value]]): Table = {
  //   Table(columns, (data ++ rows).distinct)
  // }
  override def addRows(table: Table): Table = {
    Table(columns, (data ++ table.data).distinct)
  }

  override def bind(column: String, v: Value): Table = {
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

  // def project(col: String): Table = {
  //   data.map { row =>
  //     val colIdx = columns.indexOf(col)
  //     if (colIdx > -1) row(colIdx)
  //     else throw new IllegalArgumentException(s"Cannot project column $col out of table with columns ${columns.mkString(", ")}")
  //   }
  // }

  override def project(cols: Seq[String]): Table = {
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

  override def rearrangeColumns(cols: Seq[String]): Table = {
    val colsIdx = cols.map(columns.indexOf)
    val newData = data.map { row =>
      colsIdx.map(row.apply)
    }
    Table(cols, newData)
  }

  override def join(other: Table): Table = {
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
