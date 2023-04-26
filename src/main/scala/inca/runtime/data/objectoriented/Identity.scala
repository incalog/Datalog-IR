package inca.runtime.data.objectoriented

abstract class Identity(val typ: String) extends truechange.URI {
  def readField[T](name: String): T = {
    throw new RuntimeException(s"Can not directly read a field of none SID object: $this!")
  }

  def isNull: Boolean = false
}