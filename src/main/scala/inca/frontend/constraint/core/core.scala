package inca.frontend.constraint

import inca.compiler.SourceLocation


package object core {
  val Continue: CoreStatement = FailStatement
  val TUnit: TTuple = TTuple(Seq.empty)

  case class Path(path: String) extends SourceLocation {
    override def toString: String = path
  }

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
