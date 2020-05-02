package org.inca.lang

import org.inca.meta.MetaElements._

object Core {

  // content
  trait Pattern extends Named {
    val parameters: Seq[Parameter]
    val bodies: Seq[PatternBody]
    val visibility: Option[PatternVisibility]
  }

  trait PatternBodyContent

  trait PatternVisibility

  trait Parameter extends Variable

  trait PatternBody {
    val contents: Seq[PatternBodyContent]
  }

  trait Named {
    val name: String
  }

  // variables
  abstract class AbstractTemporaryVariable(name: String, typ: Option[NodeType])

  case class TemporaryVariable(name: String, typ: Option[NodeType])
    extends AbstractTemporaryVariable(name, typ) with Variable with VariableValue

  // values
  trait Value

  trait VariableValue extends Value

  trait Variable extends Named {
    val typ: Option[NodeType]
  }

  // references
  case class PatternCall(transitive: Boolean, arguments: Seq[Value], pattern: Pattern)

  abstract class AbstractVariableReference(variable: Variable)

  case class VariableReference(variable: Variable)
    extends AbstractVariableReference(variable) with VariableValue

  // primitives
  trait LiteralValue extends Value {
    val value: Any
  }

  case class BooleanLiteral(value: Boolean) extends LiteralValue

  case class IntegerLiteral(value: Int) extends LiteralValue

  case class LongLiteral(value: Long) extends LiteralValue

  case class StringLiteral(value: String) extends LiteralValue

  // todo not yet implemented
  case class EnumLiteral(value: EnumValueLiteral) extends LiteralValue

  case class EnumValueLiteral(value: String) extends LiteralValue

}
