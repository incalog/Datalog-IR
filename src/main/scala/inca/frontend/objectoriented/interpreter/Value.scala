package inca.frontend.objectoriented.interpreter

sealed trait Value {
  def isUnit: Boolean = false
  def asBoolean: Option[Boolean] = None
  def asObject: Option[(String, Option[Int], Map[String, Value])] = None
  def updateObject(fname: String, fval: Value): Unit = {}
}
final case class ScalaValue(v: Any) extends Value {
  override def asBoolean: Option[Boolean] = v match {
    case b: Boolean => Some(b)
    case _ => None
  }
}
final case class Tuple(values: Seq[Value]) extends Value {
  override def isUnit: Boolean = values.isEmpty
}
//final case class Set(values: Seq[Value])
final case class ObjectValue(cls: String, id: Int, fvals: Map[String, Value]) extends Value {
  override def asObject: Option[(String, Option[Int], Map[String, Value])] = Some(cls, Some(id), fvals)
  override def updateObject(fname: String, fval: Value): Unit = fvals += fname -> fval
}
final case class StructuralObjectValue(cls: String, fvals: Map[String, Value]) extends Value {
  override def asObject: Option[(String, Option[Int], Map[String, Value])] = Some(cls, None, fvals)
  override def updateObject(fname: String, fval: Value): Unit = fvals += fname -> fval
}
final case class Address(index: Int) extends Value


object Address {
  val nullPtr: Address = Address(-1)
}

object Value {
  val unit: Tuple = Tuple(Seq())
}
