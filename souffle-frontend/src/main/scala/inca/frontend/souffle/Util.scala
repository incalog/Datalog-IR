package inca.frontend.souffle

import inca.frontend.souffle.Syntax.Name

object Util {

  def cleanSouffleName(name: Name): String = {
    val s = name.name
    val trimmed = if (s.startsWith("?"))
      s.substring(1)
    else
      s
    trimmed.replace("?", "")
  }
}
