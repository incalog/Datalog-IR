package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.util.Meta
import inca.util.Meta.Scala

import scala.meta.Term

object Core {
  type Module = inca.frontend.core.Module

  /*
   * data language constructs
   */

  case class DataOp(qualifier: Option[Name],
                    operation: Name,
                    isAssociative: Boolean = false,
                    isCommutative: Boolean = false) extends SourceLocation {
    def prettyprint: String = qualifier match {
      case Some(q) => q + "." + operation
      case None => operation.name
    }

    def asScala: Scala[Term] = Scala(Meta.mkQualName(prettyprint))
  }



}
