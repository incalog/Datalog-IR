package inca.frontend.objectoriented.interpreter

trait Store {
  def malloc(): Int
  def updated(index: Int, v: Value): Unit
  def lookup(index: Int): Option[Value]
  def gc(liveRefs: Set[Int]): Unit
}

