package inca.frontend.objectoriented.interpreter

trait Store {
  def malloc(): Int
  def update(index: Int, v: Value): Unit
  def lookup(index: Int): Option[Value]
  def gc(liveRefs: Set[Int]): Unit
}

// TODO: Implement

class SimpleStore(maxSize: Int = 1024) extends Store {
  private val memory = Array.ofDim[Value](maxSize)

  var free : Int = maxSize

  var nextFreeAddr : Int = 0

  override def malloc() : Int = {
    if (free <= 0) sys.error("out of memory")

    while (memory(nextFreeAddr) != null) {
      nextFreeAddr += 1
      if (nextFreeAddr == maxSize) nextFreeAddr = 0
    }

    free -= 1
    nextFreeAddr
  }

  override def update(index: Int, v: Value) : Unit = {
    memory.update(index, v)
  }

  override def lookup(index: Int): Option[Value] = {
    if (index >= 0 && index < maxSize)
      Some(memory(index))
    else
      None
  }

  override def gc(liveRefs: Set[Int]): Unit = ???
}