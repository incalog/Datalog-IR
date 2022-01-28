package inca.compiler.source

trait SourceLocation {
  var source: Source = NoSource
  var startIndex: Int = SourceLocation.NoIndex
  var endIndex: Int = SourceLocation.NoIndex

  def sourceObject: SourceObject = new SourceObject(this)
  def sourceExcerpt(config: ExcerptConfig): SourceExcerpt = new SourceExcerpt(this, config)

  def sourceLocFrom(other: SourceLocation): this.type = {
    this.source = other.source
    this.startIndex = other.startIndex
    this.endIndex = other.endIndex
    this
  }
}
object SourceLocation {
  val NoIndex: Int = -1
  object NoSourceLocation extends SourceLocation
}

case class SourceLocationList(list: Seq[SourceLocation]) extends SourceLocation {
  this.source = list.head.source
  this.startIndex = list.map(_.startIndex).min
  this.endIndex = list.map(_.endIndex).min
}