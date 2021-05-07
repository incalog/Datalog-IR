package inca.frontend.souffle

object Util {

  def cleanSouffleName(s: String): String = {
    val trimmed = if (s.startsWith("?"))
      s.substring(1)
    else
      s
    trimmed.replace("?", "")
  }
}
