package inca.frontend.souffle

import inca.compiler.source.SourceLocation

object Syntax {

  case class SouffleModule(contents: Seq[SouffleContent]) {
    lazy val rules: Map[String, Seq[(RuleHead, RuleDefinition)]] = {
      val ruleNames = contents.flatMap {
        case RuleDefinition(heads, _) => heads.map(_.rule.name)
        case _ => Seq()
      }
      (for (name <- ruleNames)
        yield name -> rulesByName(name)).toMap
    }

    def rulesByName(name: String): Seq[(RuleHead, RuleDefinition)] =
      contents.flatMap {
        case rule@RuleDefinition(heads, body) =>
          heads.filter(_.rule.name == name).map(_ -> rule)
        case _ => Seq()
      }
  }

  case class Name(name: String) extends SourceLocation {
    override def toString: String = name
  }

  sealed trait SouffleContent extends SourceLocation
  case class ComponentInitialization(name: Name, composite: Name) extends SouffleContent
  case class ComponentDefinition(name: Name, contents: Seq[SouffleContent]) extends SouffleContent
  case class TypeDeclaration(name: Name, assignedType: Option[DeclaredType]) extends SouffleContent

  def cleanRuleName(name: Name): String = name.toString.replaceAllLiterally("$", "__")
  def cleanVarName(name: Name): String = name.toString.replaceAllLiterally("$", "__")

  case class RuleSignature(name: Name, parameters: Seq[RuleParameter], output: Boolean) extends SouffleContent {
    override def toString: String = {
      val outputS = if (output) ".output " else ""
      s".decl ${cleanRuleName(name)}(${parameters.map(_.toString).mkString(", ")})"
    }
  }
  case class RuleParameter(name: Name, typ: Type) {
    override def toString: String = s"${cleanVarName(name)}: $typ"
  }

  case class Output(name: Name) extends SouffleContent {
    override def toString: String = s".output ${cleanRuleName(name)}"
  }
  case class PrintSize(name: Name) extends SouffleContent

  case class Input(rule: Name, filename: String, delimiter: String) extends SouffleContent {
    override def toString: String = ".input " + cleanRuleName(rule)
  }


  case class RuleBody(ss: Seq[Statement]) extends SourceLocation {
    override def toString: String = ss.map(_.toString).mkString(", ")
  }
  case class RuleDefinition(heads: Seq[RuleHead], body: RuleBody) extends SouffleContent {
    override def toString: String = {
      val headsS = heads.map(_.toString).mkString(", ")
      s"$headsS :- $body."
    }
  }
  case class RuleHead(rule: Name, arguments: Seq[Expression]) extends SourceLocation {
    override def toString: String = s"${cleanRuleName(rule)}(${arguments.map(_.toString).mkString(", ")})"
  }

  sealed trait Statement extends SourceLocation
  case class RelationApplication(negated: Boolean, component: Option[Name], rel: Name, arguments: Seq[Expression]) extends Statement {
    override def toString: String = {
      val negS = if (negated) "!" else ""
      s"$negS${cleanRuleName(rel)}(${arguments.map(_.toString).mkString(", ")})"
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
  sealed trait Expression extends SourceLocation
  case class Variable(name: Name) extends Expression {
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

  case class ADTValue(name: Name, args: Seq[Expression]) extends Expression {
    override def toString: String = {
      val argsString =
        if (args.isEmpty) ""
        else s"(${args.map(_.toString).mkString(", ")})"
      "$" + cleanRuleName(name) + argsString
    }
  }

  case class BuiltInFunctionCall(fun: BuiltInFunction, arguments: Seq[Expression]) extends Expression {
    override def toString: String = fun match {
      case func: InfixBuiltInFunction => s"${arguments.head.toString} $func ${arguments(1).toString}"
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
  case class DeclaredType(name: Name) extends Type {
    override def toString: String = name.toString
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

  def collectNames(rule: RuleDefinition): Set[Name] = {
    rule.heads.flatMap(collectNames).toSet ++ rule.body.ss.flatMap(collectNames)
  }

  def collectNames(head: RuleHead): Set[Name] = head.arguments.flatMap(collectNames).toSet

  def collectNames(exp: Expression): Set[Name] = exp match {
    case Variable(name) => Set(name)
    case StringValue(_) => Set()
    case NumberValue(_) => Set()
    case Syntax.Wildcard => Set()
    case BuiltInFunctionCall(_, arguments) =>
      // TODO only cat function supported
      Set(Name("cat")) ++ arguments.flatMap(collectNames)
  }

  def collectNames(stm: Statement): Set[Name] = stm match {
    case RelationApplication(negated, component, rule, arguments) =>
      Set(rule) ++ arguments.flatMap(collectNames)
    case Equality(left, _, right) => collectNames(left) ++ collectNames(right)
  }
}