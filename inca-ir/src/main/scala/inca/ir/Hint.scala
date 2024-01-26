package inca.ir

import scala.collection.mutable

trait Hints {
  private val hints: mutable.Map[HintKey[_], Hint] = mutable.Map()
  def addHint(hint: Hint*): this.type = {
    hint.foreach(h => hints += h.key -> h)
    this
  }
  def withHints(h: Hints): this.type = {
    this.hints.clear()
    this.hints ++= h.hints
    this
  }
  def hasHint(key: HintKey[_]): Boolean = this.hints.contains(key)
//  def hasHint(hint: Hint): Boolean = this.hints.contains(hint.key)


  def getHint[T <: Hint](key: HintKey[T]): Option[T] = this.hints.get(key) match
    case Some(value) => Some(value.asInstanceOf[T])
    case None => None
}

trait HintKey[H <: Hint]
trait Hint {
  def key: HintKey[_]
}
object Hint {
  def preserveHints[T <: Hints](hints: Hints)(f: => Seq[T]): Seq[T] = {
    val t = f
    t.map(_.withHints(hints))
  }

  def preserveHints[T <: Hints](hints: Hints)(f: => T): T = {
    val t = f
    t.withHints(hints)
  }
}
