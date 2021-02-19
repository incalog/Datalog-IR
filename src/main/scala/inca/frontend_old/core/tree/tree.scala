package inca.frontend_old.core

import inca.compiler.SourceLocation

package object tree {
  val Continue: CoreStatement = FailStatement
  val TUnit: TTuple = TTuple(Seq.empty)

  case class Name(name: String) extends SourceLocation {
    override def toString: String = name
  }

  sealed trait Visibility extends SourceLocation {
    def prettyprint(implicit indent: String): String
  }

  case object Private extends Visibility {
    def prettyprint(implicit indent: String): String = "private"
  }

}
