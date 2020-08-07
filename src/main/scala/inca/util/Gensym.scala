package inca.util

class Gensym(init: Iterable[String]) {
  /** map of used symbols, each of which must end with '_' */
  private var used: Map[String, Int] = Map()

  init.foreach(register)

  def register(it: Iterable[String]): Unit =
    it.foreach(register)

  def register(s: String): Unit = {
    val ix = s.lastIndexOf('_')
    if (ix <= 0) {
      val s_ = ensureUnder(s)
      used += s_ -> used.getOrElse(s_, 0)
    } else {
      val digits = s.substring(ix + 1)
      digits.toIntOption match {
        case Some(num) =>
          val s_ = s.substring(0, ix+1)
          used += s_ -> num.max(used.getOrElse(s_, 0))
        case None =>
          val s_ = ensureUnder(s)
          used += s_ -> used.getOrElse(s_, 0)
      }
    }

    val digits = s.reverse.takeWhile(_.isDigit).reverse
    if (digits.length == 0) {
      used += s -> used.getOrElse(s, 0)
    } else {
      val count = digits.toInt + 1
      val prefix = s.substring(0, s.length  - digits.length)
      used += prefix -> count.max(used.getOrElse(s, 0))
    }
  }

  def fresh(base: String): String = {
    val base_ = ensureUnder(base)
    used.get(base_) match {
      case Some(count) =>
        val v = base_ + count
        used += base_ -> (count + 1)
        v
      case None =>
        used += base_ -> 0
        base
    }
  }

  private def ensureUnder(s: String): String =
    if (s.endsWith("_") && s != "_")
      s
    else
      s + "_"

  def scoped[A](f: => A): A = {
    val oldused = this.used
    try {
      val a = f
      a
    } finally {
      this.used = oldused
    }
  }
}
