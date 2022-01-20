package inca.debugger.table

// first implementation, we do not consider efficiency
case class SimpleTable[V](columns: Vector[String], rows: Vector[Vector[V]]) extends Table[V] {

  private val columnIdx: Map[String, Int] = columns.zipWithIndex.toMap
  // private val inverseColumnsIdx: Map[Int, String] = columnIdx.map { case (c, i) => (i, c) }

  override def isEmpty: Boolean = rows.isEmpty
  override def isBound(col: String): Boolean = columns.contains(col)

  override def renameColumns(columnsSubst: Map[String, String]): SimpleTable[V] = {
    SimpleTable(columns.map(columnsSubst.apply), rows)
  }

  override def addRow(row: Seq[V]): SimpleTable[V] = {
    val newData =
      if (rows.contains(row))
        rows
      else
        rows :+ row.toVector
    SimpleTable(columns, newData)
  }

  override def addRows(table: Table[V]): SimpleTable[V] =
    SimpleTable(columns, (rows ++ table.rows.map(_.toVector)).distinct)

  override def bind(column: String, vs: Seq[V]): SimpleTable[V] = ???

  override def bind(column: String, v: V): SimpleTable[V] = {
    val colIdx = columns.indexOf(column)
    if (colIdx > -1) {
      val newData = rows.filter { row =>
        row(colIdx) == v
      }
      SimpleTable(columns, newData)
    } else {
//      val newData = {
//        if (data.isEmpty)
//          Vector(Vector(v))
//        else
//          data.map { row =>
//            row :+ v
//          }
//      }
      val newData = rows.map { row =>
        row :+ v
      }
      SimpleTable(columns :+ column, newData)
    }
  }

  override def addColumn(column: String): SimpleTable[V] = {
    SimpleTable(columns :+ column, rows)
  }

  override def project(cols: Seq[String]): SimpleTable[V] = {
    val newColumns = cols.filter(columns.contains).toVector
    val newData = rows.map { row =>
      newColumns.flatMap { col =>
        val colIdx = columns.indexOf(col)
        if (colIdx < 0 || colIdx >= row.size)
          Vector()
        else
          Vector(row(colIdx))
      }
    }
    SimpleTable(newColumns, newData)
  }

  override def rearrangeColumns(cols: Seq[String]): SimpleTable[V] = {
    val colsVector = cols.toVector
    val colsIdx = colsVector.map(columns.indexOf)
    val newData = rows.map { row =>
      colsIdx.map(row.apply)
    }
    SimpleTable(colsVector, newData)
  }

  override def join(other: Table[V]): SimpleTable[V] = {
    val otherCols = other.columns.diff(columns)
    val otherColsIdx = otherCols.map(other.columns.indexOf)
    val newColumns = columns ++ otherCols
    val sameCols = columns.filter(other.columns.contains)
    val sameColIdxs = sameCols.map(columns.indexOf)
    val otherSameColIdxs = sameCols.map(other.columns.indexOf)
    val newData = rows.flatMap { row =>
      val sameColVals = sameColIdxs.map(row.apply)
      other.rows.filter { otherRow =>
        val otherSameColVals = otherSameColIdxs.map(otherRow.apply)
        sameColVals == otherSameColVals
      }.map { otherRow =>
        row ++ otherColsIdx.map(otherRow.apply)
      }
    }
    SimpleTable(newColumns, newData)
  }

  override def columnIndex(col: String): Int = columnIdx.getOrElse(col, -1)

  override def filter(pred: Seq[V] => Boolean): SimpleTable[V] = {
    val newData = rows.filter(pred)
    SimpleTable(columns, newData)
  }

  override def map(f: Seq[V] => Seq[V]): SimpleTable[V] = {
    val newData = rows.map(row => f(row).toVector)
    SimpleTable(columns, newData)
  }

  override def expand(newcol: String, f: Seq[V] => V): SimpleTable[V] = {
    val newData = rows.map(row => row :+ f(row))
    SimpleTable(columns :+ newcol, newData)
  }

  override def expand(newcols: Seq[String], f: Seq[V] => Seq[V]): SimpleTable[V] = {
    val newData = rows.map(row => row ++ f(row))
    SimpleTable(columns ++ newcols, newData)
  }

  override def flatMap(f: Seq[V] => Seq[Seq[V]]): SimpleTable[V] = {
    val newData = rows.flatMap(row => f(row).map(_.toVector))
    SimpleTable(columns, newData)
  }

  override def contains(colValPairs: Seq[(String, V)]): Boolean = {
    rows.exists { row =>
      colValPairs.forall { case (col, v) =>
        val colIdx = columnIndex(col)
        row(colIdx) == v
      }
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: Table[V] =>
      this.columns == other.columns &&
        other.rows.forall { row =>
          this.rows.contains(row)
        }
    case _ => false
  }
}
