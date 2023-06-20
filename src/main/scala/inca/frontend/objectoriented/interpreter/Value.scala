package inca.frontend.objectoriented.interpreter

sealed trait Value
final case class ScalaValue(v: Any) extends Value
//final case class Tuple(values: Seq[Value])
//final case class Set(values: Seq[Value])
final case class ObjectValue(cls: String, id: Int, fvals: Seq[Value]) extends Value
final case class StructuralObjectValue(cls: String, fvals: Seq[Value]) extends Value
final case class Address(index: Int) extends Value


object Address {
  val nullPtr: Address = Address(-1)
}
