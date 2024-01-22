package inca.util

trait Design:
  def top: (String, String, String, String)
  def header: (String, String, String, String)
  def bottom: (String, String, String, String)
  def verticalLine: String

case object Classic extends Design:
  override val top: (String, String, String, String) = ("+", "+", "+", "-")
  override val header: (String, String, String, String) = ("+", "+", "+", "-")
  override val bottom: (String, String, String, String) = ("+", "+", "+", "-")
  override val verticalLine: String = "|"

case object Fancy extends Design:
  override val top: (String, String, String, String) = ("┌", "┬", "┐", "─")
  override val header: (String, String, String, String) = ("╞", "╪", "╡", "═")
  override val bottom: (String, String, String, String) = ("└", "┴", "┘", "─")
  override val verticalLine: String = "│"

case object Markdown extends Design:
  override val top: (String, String, String, String) = ("", "", "", "")
  override val header: (String, String, String, String) = ("|", "|", "|", "-")
  override val bottom: (String, String, String, String) = ("", "", "", "")
  override val verticalLine: String = "|"

object Tabulator {
  val defaultDesign: Design = Fancy

  def format(title: String, header: Seq[String], table: Seq[Seq[Any]], design: Design = defaultDesign): String = {
    val completeTable = Seq(header) ++ table
    completeTable match {
      case Seq() => ""
      case _ =>
        val sizes = for (row <- completeTable) yield {
          for (cell <- row) yield
            if (cell == null)
              0
            else
              cell.toString.length
        }
        val colSizes = for (col <- sizes.transpose) yield col.max
        val rows = for (row <- completeTable) yield formatRow(row, colSizes, design)
        formatRows(title, colSizes, rows, design)
    }
  }

  private def formatRows(title: String, colSizes: Seq[Int], rows: Seq[String], design: Design): String = (
    title ::
      rowSeparator(colSizes, design.top) ::
      rows.head ::
      rowSeparator(colSizes, design.header) ::
      rows.tail.toList :::
      rowSeparator(colSizes, design.bottom) ::
      List()).mkString("\n")

  private def formatRow(row: Seq[Any], colSizes: Seq[Int], design: Design): String = {
    val vertical = design.verticalLine
    val cells = for ((item, size) <- row.zip(colSizes)) yield
      if (size == 0)
        ""
      else
        (s"%-" + size + "s").format(item)
    cells.mkString(s"$vertical ", s" $vertical ", s" $vertical")
  }

  private def rowSeparator(colSizes: Seq[Int], leftMiddleRightLine: (String, String, String, String)): String =
    val (left, middle, right, line) = leftMiddleRightLine
    colSizes.map(s => line * (s + 2)).mkString(left, middle, right)
}
