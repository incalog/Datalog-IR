package inca.souffle

object Syntax {

  case class Analysis(contents: Seq[AnalysisContent])

  sealed trait AnalysisContent
  case class ComponentInitialization(name: String, composite: String) extends AnalysisContent
  case class ComponentDefinition(name: String, contents: Seq[AnalysisContent]) extends AnalysisContent
  case class TypeDeclaration(name: String, superType: Option[DeclaredType]) extends AnalysisContent

  case class RuleSignature(name: String, parameters: Seq[RuleParameter], output: Boolean) extends AnalysisContent
  case class RuleParameter(name: String, typ: Type)

  case class Output(name: String) extends AnalysisContent
  case class PrintSize(name: String) extends AnalysisContent

  case class Input(rule: String, filename: String, delimiter: String) extends AnalysisContent

  case class RuleDefinition(heads: Seq[RuleHead], body: Seq[Statement]) extends AnalysisContent
  case class RuleHead(rule: String, arguments: Seq[Expression])

  sealed trait Statement
  case class RuleApplication(negated: Boolean, component: Option[String], rule: String, arguments: Seq[Expression]) extends Statement
  case class Equality(left: Expression, not: Boolean, right: Expression) extends Statement
  case class Or(left: Statement, right: Statement) extends Statement
  case class Parens(stm: Statement) extends Statement

  sealed trait Expression
  case class Variable(name: String) extends Expression
  case class StringValue(value: String) extends Expression
  case class NumberValue(value: Int) extends Expression
  case object Any extends Expression
  case class BuiltInFunctionCall(fun: BuiltInFunction, arguments: Seq[Expression]) extends Expression

  sealed trait BuiltInFunction
  case object CatBuiltInFunction extends BuiltInFunction

  sealed trait Type
  case class DeclaredType(name: String) extends Type

  sealed trait PrimitiveType extends Type
  case object SymbolType extends PrimitiveType
  case object NumberType extends PrimitiveType
  case object UnsignedType extends PrimitiveType
  case object FloatType extends PrimitiveType
}