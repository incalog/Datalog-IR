package inca.debugger.table.indexing

case class IndexCover(order: Seq[String]) {
  def toIndexOrder(cols: Seq[String]): IndexOrder =
    IndexOrder(order.map(cols.indexOf))
}
