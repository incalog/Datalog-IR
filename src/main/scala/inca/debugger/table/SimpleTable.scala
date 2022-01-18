package inca.debugger.table

import inca.debugger.Value

// first implementation, we do not consider efficiency
case class SimpleTable(columns: Vector[String], data: Vector[Vector[Value]]) extends Table {

  private val columnIdx: Map[String, Int] = columns.zipWithIndex.toMap
  // private val inverseColumnsIdx: Map[Int, String] = columnIdx.map { case (c, i) => (i, c) }

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

  override def addRows(table: Table): Table = {
    Table(columns, (data ++ table.data).distinct)
  }

  override def bind(column: String, vs: Seq[Value]): Table = ???

  override def bind(column: String, v: Value): Table = {
    val colIdx = columns.indexOf(column)
    if (colIdx > -1) {
      val newData = data.filter { row =>
        row(colIdx) == v
      }
      Table(columns, newData)
    } else {
//      val newData = {
//        if (data.isEmpty)
//          Vector(Vector(v))
//        else
//          data.map { row =>
//            row :+ v
//          }
//      }
      val newData = data.map { row =>
        row :+ v
      }
      Table(columns :+ column, newData)
    }
  }

  override def addColumn(column: String): Table = {
    Table(columns :+ column, data)
  }

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

  override def columnIndex(col: String): Int = columnIdx(col)

  override def filter(pred: Seq[Value] => Boolean): Table = {
    val newData = data.filter(pred)
    Table(columns, newData)
  }

  override def map(f: Seq[Value] => Seq[Value]): Table = {
    val newData = data.map(f)
    Table(columns, newData)
  }

  override def flatMap(f: Seq[Value] => Seq[Seq[Value]]): Table = {
    val newData = data.flatMap(f)
    Table(columns, newData)
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: Table =>
      this.columns == other.columns &&
        this.data.forall { row =>
          other.data.contains(row)
        }
    case _ => false
  }
}
