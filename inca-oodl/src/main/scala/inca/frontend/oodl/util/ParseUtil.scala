package inca.frontend.oodl.util

import inca.ir.Name
import inca.ir.util.SourceLocation

object ParseUtil {
  def parseTupleIndex(name: Name): Option[Int] =
    val startsWithUnderscore = name.name.startsWith("_")
    try {
      Some(name.name.substring(1).toInt)
    } catch {
      case e: Exception => None
    }
}
