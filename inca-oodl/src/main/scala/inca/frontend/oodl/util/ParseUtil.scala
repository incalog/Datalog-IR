package inca.frontend.oodl.util

import inca.ir.Name
import inca.ir.util.SourceLocation

object ParseUtil {
  def parseTupleIndex(name: Name): Option[Int] =
    try {
      Some(name.name.substring(1).toInt)
    } catch {
      case e: Exception => None
    }
}
