package inca.compiler

trait SourceLocation {
  var startIndex: Int = SourceLocation.NoIndex
  var endIndex: Int = SourceLocation.NoIndex
  def sourceObject: SourceObject = new SourceObject(this)
}
object SourceLocation {
  val NoIndex: Int = -1
  object NoSourceLocation extends SourceLocation
}

class SourceObject(val o: SourceLocation) {
  override def equals(obj: Any): Boolean = obj match {
    case that: SourceObject => o.startIndex == that.o.startIndex && o.endIndex == that.o.endIndex && o == that.o
    case _ => false
  }

  override def hashCode(): Int = (o.startIndex, o.endIndex, o).hashCode()
}