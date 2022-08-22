package inca.compiler

import java.util.Objects.hash

trait DotPrintable {
  val nodeName: String
  val nodeId: Int
  def nodeShape: String = "plaintext"

  def dotString: String =
    s"""$nodeId [label="$nodeName", shape=$nodeShape];\n"""
}

trait SourceLocation extends DotPrintable {
  var startIndex: Int = SourceLocation.NoIndex
  var endIndex: Int = SourceLocation.NoIndex
  def location: (Int, Int) = (this.startIndex, this.endIndex)

  lazy val nodeName: String = this.getClass.getSimpleName
  lazy val nodeId: Int = hash(this.location, this.nodeName)
}
object SourceLocation {
  val NoIndex: Int = -1
  object NoSourceLocation extends SourceLocation
}
