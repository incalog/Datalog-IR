package inca.frontend.util

object Tabulator {
  def format(title: String, header: Seq[String], table: Seq[Seq[Any]]): String = {
    val completeTable = Seq(header) ++ table
    completeTable match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- completeTable) yield {
          for (cell <- row)
            yield
              if (cell == null)
                0
              else
                cell.toString.length
        }
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- completeTable) yield formatRow(row, colSizes)
        formatRows(title, rowSeparator(colSizes), rows)
    }
  }

  def formatRows(title: String, rowSeparator: String, rows: Seq[String]): String = (title ::
    rowSeparator ::
    rows.head ::
    rowSeparator ::
    rows.tail.toList :::
    rowSeparator ::
    List()).mkString("\n")

  def formatRow(row: Seq[Any], colSizes: Seq[Int]): String = {
    val cells =
      for ((item, size) <- row.zip(colSizes))
        yield
          if (size == 0)
            ""
          else
            ("%-" + size + "s").format(item)
    cells.mkString("| ", " | ", " |")
  }

  def rowSeparator(colSizes: Seq[Int]): String = colSizes.map { s =>
    "-" * (s + 2) // + 2 for left and right space padding
  }.mkString("+", "+", "+")
}
