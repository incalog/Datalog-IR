package inca.embedded2

trait Fun[FL <: Language[_]] extends Language[FL] {
  val fl: FL
  trait Value
  case class FLVal(v: fl.Value) extends Value
  case class SetVal(v: Seq[Value]) extends Value
  case class TupleVal(v: Seq[Value]) extends Value
  case class ADTVal(ctor: String, args: Seq[Value]) extends Value
  // ...
}
