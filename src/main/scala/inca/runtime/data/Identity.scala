package inca.runtime.data

class Identity(val typ: String) extends truechange.URI {
  def readField[T](name: String): T = {
    throw new RuntimeException(s"Can not directly read a field of none SID object: $this!")
  }

  val isNull: Boolean = this.typ == "Null"
}

case class NullID() extends Identity("Null")

object NullID {
  def apply(): Identity = {
    new NullID()
  }
}