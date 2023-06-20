package inca.frontend.objectoriented.interpreter

trait Store {
  def malloc(): Int
  def update(index: Int, v: Value): Unit
  def lookup(index: Int): Option[Value]
  def gc(liveRefs: Set[Int]): Unit
}

// TODO: Implement
class SimpleStore() extends Store {
  override def malloc(): Int = ???
  override def update(index: Int, v: Value): Unit = ???
  override def lookup(index: Int): Option[Value] = ???
  override def gc(liveRefs: Set[Int]): Unit = ???
}