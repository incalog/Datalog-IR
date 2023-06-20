package inca.frontend.objectoriented.interpreter

sealed trait Value {
  def asBool: Option[Boolean] = None
}
final case class ScalaValue(v: Any) extends Value {
  override def asBool: Option[Boolean] = this match {
    case ScalaValue(true) => Some(true)
    case ScalaValue(false) => Some(false)
    case _ => None
  }
}
final case class ObjectValue(cls: String, id: Int, fvals: Seq[Value]) extends Value
final case class StructuralObjectValue(cls: String, fvals: Seq[Value]) extends Value
final case class Address(index: Int) extends Value

object Address {
  val nullPtr: Address = Address(-1)
}
