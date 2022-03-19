package inca.frontend.constraint.core

import inca.compiler.source.SourceLocation

sealed trait Visibility extends SourceLocation {
  def prettyprint(implicit indent: String): String
}

case object Private extends Visibility {
  def prettyprint(implicit indent: String): String = "private"
}

