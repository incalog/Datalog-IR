package inca.frontend.constraint.core

import inca.compiler.source.SourceLocation

case class Name(name: String) extends SourceLocation {
  override def toString: String = name
}
