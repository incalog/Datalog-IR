package inca.frontend.constraint.core

import inca.compiler.SourceLocation


sealed trait Literal extends SourceLocation {
  def prettyprint: String
}
case object UnitLiteral extends Literal {
  override def prettyprint: String = "unit"
}
case class BooleanLiteral(v: Boolean) extends Literal {
  override def prettyprint: String = v.toString
}
case class IntLiteral(v: Int) extends Literal {
  override def prettyprint: String = v.toString
}
case class LongLiteral(v: Long) extends Literal {
  override def prettyprint: String = v.toString
}
case class DoubleLiteral(v: Double) extends Literal {
  override def prettyprint: String = v.toString
}
case class StringLiteral(v: String) extends Literal {
  override def prettyprint: String = '\"' + v + '\"'
}