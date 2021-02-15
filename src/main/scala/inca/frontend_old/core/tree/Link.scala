package inca.frontend_old.core.tree

import inca.frontend_old.parser.SourceLocation

sealed trait Link extends SourceLocation {
  def prettyprint: String
}

sealed trait CoreLink extends Link

case class NamedLink(field: Name) extends CoreLink {
  override def prettyprint: String = field.name
}

object NamedLink {
  def apply(field: String): NamedLink = new NamedLink(Name(field))
}

case object ParentLink extends CoreLink {
  override def prettyprint: String = "parent"
}

case object ChildrenLink extends CoreLink {
  override def prettyprint: String = "children"
}

case object NextLink extends CoreLink {
  override def prettyprint: String = "next"
}

case object PreviousLink extends CoreLink {
  override def prettyprint: String = "prev"
}

case object SizeLink extends CoreLink {
  override def prettyprint: String = "size"
}