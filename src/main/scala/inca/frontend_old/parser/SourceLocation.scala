package inca.frontend_old.parser

trait SourceLocation {
  var startIndex: Int = SourceLocation.NoIndex
  var endIndex: Int = SourceLocation.NoIndex
}
object SourceLocation {
  val NoIndex: Int = -1
  object NoSourceLocation extends SourceLocation
}
