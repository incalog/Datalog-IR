package inca.util

import scala.collection.mutable

class Gensym(init: Set[String]) {
  val used: mutable.Map[String, Int] = mutable.Map()

  init.foreach(register)

  def register(s: String): Unit = {
    val digits = s.reverse.takeWhile(_.isDigit).reverse
    if (digits.length == 0) {
      used(s) = used.getOrElse(s, 0)
    } else {
      val count = digits.toInt + 1
      val prefix = s.substring(0, s.length  - digits.length)
      used(prefix) = count.max(used.getOrElse(s, 0))
    }
  }

  def fresh(base: String): String = used.get(base) match {
    case Some(count) =>
      val v = base + count
      used(base) = count + 1
      v
    case None =>
      used(base) = 0
      base
  }
}
