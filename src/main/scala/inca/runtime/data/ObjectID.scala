package inca.runtime.data

import scala.collection.mutable

case class ObjectID(override val typ: String, allocId: Int) extends Identity(typ) {
  /*
  override def toString: String = s"${super.toString}($typ, $allocId)"
  override def equals(obj: Any): Boolean = {
    println(s"Compare: $this, ${this.getClass.getSimpleName} == $obj, ${obj.getClass.getSimpleName}")
    super.equals(obj)
  }
  */
}

object ObjectID {
  def apply(typ: String, allocId: Int): ObjectID = {
    new ObjectID(typ, allocId)
  }
}
