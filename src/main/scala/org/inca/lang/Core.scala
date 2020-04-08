package org.inca.lang

import org.inca.meta.MetaElements.MetaElement

object Core {

  // content
  trait Pattern extends Named with PatternModuleContent {
    val parameters: Seq[Parameter]
    val bodies: Seq[PatternBody]
    val visibility: Option[PatternVisibility]
  }

  trait PatternBodyContent

  trait PatternModuleContent

  trait PatternVisibility

  trait Parameter extends Variable

  trait PatternBody {
    val contents: Seq[PatternBodyContent]
  }

  trait Named {
    val name: String
  }

  // variables
  abstract class AbstractTemporaryVariable(name: String, typ: Option[MetaElement])

  case class TemporaryVariable(name: String, typ: Option[MetaElement])
    extends AbstractTemporaryVariable(name, typ) with Variable with VariableValue

  // values
  trait Value

  trait VariableValue extends Value

  trait Variable extends Named {
    val typ: Option[MetaElement]
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

  // todo enum literal!!!

}
