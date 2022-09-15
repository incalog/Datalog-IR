package inca.runtime.data

case class ObjectID(typ: String, allocId: Int) extends truechange.URI {
  //override def toString: String = s"${super.toString}($typ, $allocId)"
}

object ObjectID {
  def apply(typ: String): ObjectID = {
    new ObjectID(typ, 0)
  }

  def apply(typ: String, allocId: Int): ObjectID = {
    new ObjectID(typ, allocId)
  }
}
