package inca.backend.hints

import scala.collection.mutable

trait Hints {
  val hints: mutable.Map[Hint.Key, Hint] = mutable.Map()
  def addHint(hint: Hint*): this.type = {
    hint.foreach(h => hints += h.key -> h)
    this
  }
  def withHints(h: Hints): this.type = {
    this.hints.clear()
    this.hints ++= h.hints
    this
  }
  def hasHint(key: Hint.Key): Boolean = this.hints.contains(key)
}

trait Hint {
  def key: Hint.Key
}
object Hint {
  type Key = String
}
