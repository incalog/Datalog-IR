package inca.debugger.table

// first implementation, we do not consider efficiency
case class SimpleTable[V](columns: Vector[String], rows: Vector[Vector[V]]) extends Table[V] {

  if (rows.exists(_.size != columns.size))
    throw new IllegalArgumentException

  private def constructNew(columns: Vector[String], rows: Vector[Vector[V]]): SimpleTable[V] =
    SimpleTable(columns, rows.distinct)

  private val columnIdx: Map[String, Int] = columns.zipWithIndex.toMap

  override def isEmpty: Boolean = rows.isEmpty
  override def isBound(col: String): Boolean = columns.contains(col)
  override def numRows: Int = rows.size

  override def renameColumns(columnsSubst: Map[String, String]): SimpleTable[V] = {
    constructNew(columns.map(columnsSubst.apply), rows)
  }

  override def addRow(row: Seq[V]): SimpleTable[V] = {
    val newData =
      if (rows.contains(row))
        rows
      else
        rows :+ row.toVector
    constructNew(columns, newData)
  }

  override def addRows(table: Table[V]): SimpleTable[V] =
    constructNew(columns, rows ++ table.rows.map(_.toVector))

  override def bind(column: String, v: V): SimpleTable[V] = {
    val colIdx = columns.indexOf(column)
    if (colIdx > -1) {
      val newData = rows.filter { row =>
        row(colIdx) == v
      }
      constructNew(columns, newData)
    } else {
      val newData = rows.map { row =>
        row :+ v
      }
      constructNew(columns :+ column, newData)
    }
  }

  override def project(cols: Seq[String]): SimpleTable[V] = {
    val newColumns = cols.filter(columns.contains).toVector
    val newColumnsIndex = newColumns.map(columns.indexOf)
    val newData = rows.map { row =>
      newColumnsIndex.map(row.apply)
    }
    constructNew(newColumns, newData)
  }

  override def project(columnsSubst: Map[String, String]): SimpleTable[V] = {
    val newColumns = columns.flatMap(columnsSubst.get)
    val newColumnsIndex = columns.flatMap { oldCol =>
      columnsSubst.get(oldCol) match {
        case Some(_) => Some(columnIndex(oldCol))
        case None => None
      }
    }
    val newData = rows.map { row =>
      newColumnsIndex.map(row.apply)
    }
    constructNew(newColumns, newData)
  }

  override def rearrangeColumns(cols: Seq[String]): SimpleTable[V] = {
    val colsVector = cols.toVector
    val colsIdx = colsVector.map(columns.indexOf)
    val newData = rows.map { row =>
      colsIdx.map(row.apply)
    }
    constructNew(colsVector, newData)
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
    constructNew(newColumns, newData)
  }

  override def diff(other: Table[V]): Table[V] = {
    this.filter { row =>
      val colValPair = other.columns.zip(row)
      !other.contains(colValPair)
    }
  }

  override def columnIndex(col: String): Int = columnIdx.getOrElse(col, -1)

  override def filter(pred: Seq[V] => Boolean): SimpleTable[V] = {
    val newData = rows.filter(pred)
    constructNew(columns, newData)
  }

  override def map(f: Seq[V] => Seq[V]): SimpleTable[V] = {
    val newData = rows.map(row => f(row).toVector)
    constructNew(columns, newData)
  }

  override def expand(newcol: String, f: Seq[V] => V): SimpleTable[V] = {
    val newData = rows.map(row => row :+ f(row))
    constructNew(columns :+ newcol, newData)
  }

  override def expand(newcols: Seq[String], f: Seq[V] => Seq[V]): SimpleTable[V] = {
    val newData = rows.map(row => row ++ f(row))
    constructNew(columns ++ newcols, newData)
  }

  override def flatMap(f: Seq[V] => Seq[Seq[V]]): SimpleTable[V] = {
    val newData = rows.flatMap(row => f(row).map(_.toVector))
    constructNew(columns, newData)
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
      val sameColumns = this.columns.size == other.columns.size &&
        this.columns.forall(other.columns.contains)
      val sameRows = this.numRows == other.numRows &&
        this.rows.forall { row =>
          val colValPair = this.columns.zip(row)
          other.contains(colValPair)
        }
      sameColumns && sameRows
    case _ => false
  }
}
