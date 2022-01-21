package inca.compiler.source

/** Wraps a SourceLocation and uses its coordinates for equality */
class SourceObject(val loc: SourceLocation) {
  override def equals(obj: Any): Boolean = obj match {
    case that: SourceObject =>
      loc.source == that.loc.source &&
        loc.startIndex == that.loc.startIndex &&
        loc.endIndex == that.loc.endIndex &&
        loc == that.loc
    case _ => false
  }

  override def hashCode(): Int = (loc.startIndex, loc.endIndex, loc).hashCode()

  override def toString: String = loc.toString
}