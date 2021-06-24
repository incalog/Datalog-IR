package inca.frontend.souffle

object Syntax {

  case class SouffleModule(contents: Seq[SouffleContent])

  sealed trait SouffleContent
  case class ComponentInitialization(name: String, composite: String) extends SouffleContent
  case class ComponentDefinition(name: String, contents: Seq[SouffleContent]) extends SouffleContent
  case class TypeDeclaration(name: String, assignedType: Option[DeclaredType]) extends SouffleContent

  def cleanRuleName(name: String): String = name.replaceAllLiterally("$", "__")
  def cleanVarName(name: String): String = name.replaceAllLiterally("$", "__")

  case class RuleSignature(name: String, parameters: Seq[RuleParameter], output: Boolean) extends SouffleContent {
    override def toString: String = {
      val outputS = if (output) ".output " else ""
      s".decl ${cleanRuleName(name)}(${parameters.map(_.toString).mkString(", ")})"
    }
  }
  case class RuleParameter(name: String, typ: Type) {
    override def toString: String = s"${cleanVarName(name)}: $typ"
  }

  case class Output(name: String) extends SouffleContent {
    override def toString: String = s".output ${cleanRuleName(name)}"
  }
  case class PrintSize(name: String) extends SouffleContent

  case class Input(rule: String, filename: String, delimiter: String) extends SouffleContent {
    override def toString: String = ".input " + cleanRuleName(rule)
  }


  case class RuleDefinition(heads: Seq[RuleHead], body: Seq[Statement]) extends SouffleContent {
    override def toString: String = {
      val headsS = heads.map(_.toString).mkString(", ")
      val bodyS = body.map(_.toString).mkString(", ")
      s"$headsS :- $bodyS."
    }
  }
  case class RuleHead(rule: String, arguments: Seq[Expression]) {
    override def toString: String = s"${cleanRuleName(rule)}(${arguments.map(_.toString).mkString(", ")})"
  }

  sealed trait Statement
  case class RuleApplication(negated: Boolean, component: Option[String], rule: String, arguments: Seq[Expression]) extends Statement {
    override def toString: String = {
      val negS = if (negated) "!" else ""
      s"$negS${cleanRuleName(rule)}(${arguments.map(_.toString).mkString(", ")})"
    }
  }
  case class Equality(left: Expression, not: Boolean, right: Expression) extends Statement {
    override def toString: String = {
      val op = if (not) "!=" else "="
      s"${left} $op ${right}"
    }
  }
  case class GreaterThan(left: Expression, right: Expression) extends Statement {
    override def toString: String = s"${left} > ${right}"
  }
  case class GreaterThanEqual(left: Expression, right: Expression) extends Statement {
    override def toString: String = s"${left} >= ${right}"
  }
  case class LesserThan(left: Expression, right: Expression) extends Statement {
    override def toString: String = s"${left} < ${right}"
  }
  case class LesserThanEqual(left: Expression, right: Expression) extends Statement {
    override def toString: String = s"${left} <= ${right}"
  }
//  case class Or(left: Statement, right: Statement) extends Statement
  case class Parens(stm: Statement) extends Statement {
    override def toString: String = s"($stm)"
  }

  sealed trait Expression
  case class Variable(name: String) extends Expression {
    override def toString: String = cleanVarName(name)
  }
  case class StringValue(value: String) extends Expression {
    override def toString: String = "\"" + value + "\""
  }
  case class NumberValue(value: Int) extends Expression {
    override def toString: String = value.toString
  }
  case class FloatValue(value: Float) extends Expression {
    override def toString: String = value.toString
  }
  case object Wildcard extends Expression {
    override def toString: String = "_"
  }

  case class ADTValue(name: String, args: Seq[Expression]) extends Expression {
    override def toString: String = "$" + cleanRuleName(name) + s"(${args.map(_.toString).mkString(", ")})"
  }

  case class BuiltInFunctionCall(fun: BuiltInFunction, arguments: Seq[Expression]) extends Expression {
    override def toString: String = fun match {
      case func: InfixBuiltInFunction => s"${arguments(0).toString} $func ${arguments(1).toString}"
      case func: PrefixBuiltInFunction => s"$func(${arguments.mkString(", ")})"
    }
  }

  sealed trait BuiltInFunction

  sealed trait InfixBuiltInFunction extends BuiltInFunction
  case object AddBuiltInFunction extends InfixBuiltInFunction {
    override def toString: String = "+"
  }
  case object SubBuiltInFunction extends InfixBuiltInFunction {
    override def toString: String = "-"
  }
  case object MultBuiltInFunction extends InfixBuiltInFunction {
    override def toString: String = "*"
  }
  case object DivBuiltInFunction extends InfixBuiltInFunction {
    override def toString: String = "/"
  }
  case object PowBuiltInFunction extends InfixBuiltInFunction {
    override def toString: String = "^"
  }
  case object ModBuiltInFunction extends InfixBuiltInFunction {
    override def toString: String = "%"
  }

  sealed trait PrefixBuiltInFunction extends BuiltInFunction
  case object CatBuiltInFunction extends PrefixBuiltInFunction
  case object MaxBuiltInFunction extends PrefixBuiltInFunction
  case object MinBuiltInFunction extends PrefixBuiltInFunction

  case object LandBuiltInFunction extends PrefixBuiltInFunction
  case object LorBuiltInFunction extends BuiltInFunction
  case object LxorBuiltInFunction extends BuiltInFunction
  case object LnotBuiltInFunction extends BuiltInFunction



  sealed trait Type
  case class DeclaredType(name: String) extends Type {
    override def toString: String = name
  }

  sealed trait PrimitiveType extends Type
  case object SymbolType extends PrimitiveType {
    override def toString: String = "symbol"
  }
  case object NumberType extends PrimitiveType {
    override def toString: String = "number"
  }
  case object UnsignedType extends PrimitiveType {
    override def toString: String = "unsigned"
  }
  case object FloatType extends PrimitiveType {
    override def toString: String = "float"
  }
}