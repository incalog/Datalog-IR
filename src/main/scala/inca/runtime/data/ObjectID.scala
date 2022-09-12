package inca.runtime.data

class ObjectID(typ: String) extends truechange.URI {
  override def toString: String = s"${super.toString}($typ)"
}

object ObjectID {
  def apply(typ: String): ObjectID = {
    println("Generate object: ", typ)
    new ObjectID(typ)
  }
}
