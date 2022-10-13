package inca.compiler

trait SourceLocation {
  var startIndex: Int = SourceLocation.NoIndex
  var endIndex: Int = SourceLocation.NoIndex
  def location: (Int, Int) = (this.startIndex, this.endIndex)

  def sourceObject: SourceObject = new SourceObject(this)
}
object SourceLocation {
  val NoIndex: Int = -1
  object NoSourceLocation extends SourceLocation
}
