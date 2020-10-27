package inca.frontend.parser

trait SourceLocation {
  var startIndex: Int = SourceLocation.NoIndex
  var endIndex: Int = SourceLocation.NoIndex
}
object SourceLocation {
  val NoIndex: Int = -1
}
