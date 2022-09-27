package inca.runtime.data

import scala.collection.mutable

case class ObjectID(typ: String, allocId: Int) extends truechange.URI {
  //override def toString: String = s"${super.toString}($typ, $allocId)"
  //override def equals(obj: Any): Boolean = super.equals(obj)
}

object ObjectID {

  def apply(typ: String): ObjectID = {
    new ObjectID(typ, -1)
  }

  def apply(typ: String, allocId: Int): ObjectID = {
    new ObjectID(typ, allocId)
  }
}
