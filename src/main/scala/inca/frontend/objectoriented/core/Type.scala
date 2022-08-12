// TODO: Remove all this and introduce some types that make sense
package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.Resolvable

sealed trait Type extends SourceLocation {
  def prettyprint: String
  def flatten: Seq[Type]
  override def toString: String = prettyprint
}
case object TAny extends Type {
  override def prettyprint: String = "Any"
  override def flatten: Seq[Type] = Seq(this)
}
case object TNothing extends Type {
  override def prettyprint: String = "Nothing"
  override def flatten: Seq[Type] = Seq(this)
}

case class TClass(name: Name) extends Type {
  override def prettyprint: String = name.name
  override def flatten: Seq[Type] = Seq(this)
}
