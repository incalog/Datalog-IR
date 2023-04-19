package inca.frontend.objectoriented

import inca.compiler.SourceLocation

package object core {
  val TUnit: TTuple = TTuple(Seq.empty)
  type Signature = String

  case class Name(raw: String) extends SourceLocation {
    override def toString: String = raw
  }

  case class Index(raw: Int) extends SourceLocation {
    override def toString: String = raw.toString
  }

  sealed trait Visibility extends SourceLocation {
    def prettyprint(implicit indent: String): String
  }

  case object Private extends Visibility {
    def prettyprint(implicit indent: String): String = "private"
  }
}
