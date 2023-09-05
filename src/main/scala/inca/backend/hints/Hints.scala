package inca.backend.hints

import scala.collection.mutable

trait Hints {
  private val _hints: mutable.Map[Hint.Key, Hint] = mutable.Map()
  def hints: Map[Hint.Key, Hint] = _hints.toMap
  def addHint(hint: Hint*): this.type = {
    hint.foreach(h => _hints += h.key -> h)
    this
  }
  def getHint(key: Hint.Key): Option[Hint] = _hints.get(key)
  def withHints(h: Hints): this.type = {
    this._hints.clear()
    this._hints ++= h._hints
    this
  }
  def hasHint(key: Hint.Key): Boolean = this._hints.contains(key)
}

trait Hint {
  val key: Hint.Key
}
object Hint {
  type Key = String
}
