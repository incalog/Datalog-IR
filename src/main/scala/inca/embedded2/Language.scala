package inca.embedded2

trait Language[FL <: Language[_]] {
  // syntax exposed when being used as a foreign language
  type Program
  type EntryPoint
  type TopLevelDefinition
  type Function
  type Type
  type Constant
  type InfixOperator
  type UnaryOperator

  // abstract syntax exposed when used execute
  // input of language
  type Input
  type Change
  // output of language
  type Value

  type Target
  // TODO this should be the input of the compilation target
  // def lift(input: DB): Value
  // TODO the language should not know its compilation target
}
