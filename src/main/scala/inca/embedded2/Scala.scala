package inca.embedded2

class Scala extends Language[Nothing] {
  override type Type = meta.Type
  override type Constant = meta.Term
  override type InfixOperator = meta.Term.Name
  override type UnaryOperator = meta.Term.Name
  override type Function = meta.Term.Function
  override type Value = Any
}
