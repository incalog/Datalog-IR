package inca.ir.util

import inca.ir.Name

class Gensym(init: Iterable[String] = Seq.empty) {
  /** map of used symbols, each of which must end with '$' */
  private var used: Map[String, Int] = Map()
  private var globals: Seq[String] = Seq()

  init.foreach(register)

  def register(it: Iterable[String]): Unit =
    it.foreach(register)

  def isRegistered(s: String): Boolean =
    used.contains(decompileName(s)._1)

  def register(s: String): Unit = {
    decompileName(s) match {
      case (s_, None) =>
        used += s_ -> used.getOrElse(s_, 0)
      case (s_, Some(num)) =>
        used += s_ -> (num + 1).max(used.getOrElse(s_, 0))
    }
  }

  def fresh(base: String): String = {
    val base_ = decompileName(base)._1
    used.get(base_) match {
      case Some(count) =>
        val v = base_ + count
        used += base_ -> (count + 1)
        v
      case None =>
        used += base_ -> 1
        base_ + 0
    }
  }
  
  def freshName(base: Name): Name = Name(fresh(base.name))

  def freshGlobal(base: String): String = {
    val v = fresh(base)
    globals :+= v
    v
  }

  private def decompileName(s: String): (String, Option[Int]) = {
    val ix = s.lastIndexOf('$')
    if (ix <= 0) {
      (ensureDollar(s), None)
    } else {
      val digits = s.substring(ix + 1)
      digits.toIntOption match {
        case Some(num) =>
          val s_ = s.substring(0, ix+1)
          (s_, Some(num))
        case None =>
          (ensureDollar(s), None)
      }
    }
  }

  private def ensureDollar(s: String): String =
    if (s.endsWith("$") && s != "$")
      s
    else
      s + "$"

  def scoped[A](f: => A): A = {
    val oldused = this.used
    try {
      val a = f
      a
    } finally {
      this.used = oldused
      this.globals.foreach(register)
    }
  }
}

