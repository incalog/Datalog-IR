package inca.frontend.core

import inca.frontend.parser.SourceLocation

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
  }



}
