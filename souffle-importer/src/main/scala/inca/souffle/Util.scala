package inca.souffle

object Util {

  def  cleanSouffleName(s: String): String =
    "$$" + s.replace("?", "$")
}
