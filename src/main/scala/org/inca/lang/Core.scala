package org.inca.lang

import org.inca.incer.IncrementalIndex
import org.inca.lang.Gp.VirtualPathElement
import org.inca.meta.MetaElements.MetaElement

object Core {

  // content
  trait PatternBodyContent
  trait PatternModuleContent
  trait PatternVisibility
  trait Parameter extends Variable

  trait JoinTypeDef extends NamedConcept {
    val types: Seq[MetaElement]
  }

  trait PatternBody {
    val contents: Seq[PatternBodyContent]
  }

  trait NamedConcept {
    val name: String
  }

  trait Variable extends NamedConcept {
    val typ: Option[MetaElement]
  }

  trait Pattern extends NamedConcept with PatternModuleContent {
    val parameters: Seq[Parameter]
    val bodies: Seq[PatternBody]
    val visibility: Option[PatternVisibility]
  }

  abstract class TemporaryVariable(name: String, typ: Option[MetaElement])

  case class CoreTemporaryVariable(name: String, typ: Option[MetaElement])
    extends TemporaryVariable(name, typ) with Variable with VariableValue

  // values
  trait Value

  trait VariableValue extends Value

  // todo eval func should be ` => Boolean` was `expression: Expression`
  case class ExpressionEvaluationValue(const: Boolean,
                                       unwind: Boolean,
                                       evalFunc: Boolean)
    extends Value

  // typ
  case class JoinType(joinTypeDef: JoinTypeDef) extends MetaElement

  // constraints
  trait IGeneratorPathElement extends VirtualPathElement

  trait IPatternCall {
    val transitive: Boolean
    val arguments: Seq[Value]
    val pattern: Pattern
  }

  // todo check implementation
  trait ContextPointer {
    val index: Integer
    val parent: Any
    val next: Option[Any]
    val prev: Option[Any]
    val first: Option[Any]
    val last: Option[Any]
  }

  case class PatternCall(transitive: Boolean, arguments: Seq[Value], pattern: Pattern) extends IPatternCall

  // reference
  abstract class VariableReference(variable: Variable)

  case class CoreVariableReference(variable: Variable)
    extends VariableReference(variable) with VariableValue


  trait LiteralValue extends Value {
    val value: Any
  }

  case class BooleanLiteral(value: Boolean) extends LiteralValue

  case class  IntegerLiteral(value: Int) extends LiteralValue

  case class  LongLiteral(value: Long) extends LiteralValue
  case class  StringLiteral(value: String) extends LiteralValue

}
