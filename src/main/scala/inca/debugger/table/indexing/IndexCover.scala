package inca.debugger.table.indexing

case class IndexCover(order: Seq[String]) {
  def toIndexOrder(cols: Seq[String]): IndexOrder = {
    val notMentionedCols = cols.diff(order)
    // The order of notMentionedCols is not importante but they still need to be mentoined
    val extendedOrder = order ++ notMentionedCols
    IndexOrder(extendedOrder.map(cols.indexOf))
  }
}
