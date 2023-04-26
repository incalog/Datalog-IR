package inca.runtime.data.objectoriented
case class NullID() extends Identity("Null") {
  override def isNull: Boolean = true
}

object NullID {
  def apply(): Identity = {
    new NullID()
  }
}