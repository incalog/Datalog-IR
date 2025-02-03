package inca.ir.util

var nextId: Long = 0

trait Identifiable:
  val id: Long = nextId
  nextId += 1


trait SourceLocation extends Identifiable:
  var startIndex: Int = SourceLocation.NoIndex
  var endIndex: Int = SourceLocation.NoIndex

object SourceLocation:
  val NoIndex: Int = -1

  object NoSourceLocation extends SourceLocation
