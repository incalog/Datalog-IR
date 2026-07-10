package inca.codeql.syntax

import _root_.inca.ir.Name
import _root_.inca.ir.typing.Typeable
import _root_.inca.ir.util.SourceLocation

case class Program(predicates: Seq[PredicateDecl], classes: Seq[ClassDecl], query: Option[SelectQuery]) extends SourceLocation:
  lazy val predicateMap: Map[Name, PredicateDecl] = predicates.map(p => p.name -> p).toMap
  lazy val classMap: Map[Name, ClassDecl] = classes.map(c => c.name -> c).toMap
  lazy val externalPredicates: Seq[PredicateDecl] = predicates.filter(_.external)

case class PredicateDecl(
  name: Name,
  params: Seq[VariableDecl],
  resultType: Option[QlType],
  body: Option[Formula],
  external: Boolean = false,
  query: Boolean = false
) extends SourceLocation:
  def outputTypes: Seq[QlType] = params.map(_.ty) ++ resultType.toSeq

case class VariableDecl(ty: QlType, name: Name) extends SourceLocation

case class ClassDecl(
  name: Name,
  bases: Seq[QlType],
  instanceOf: Seq[QlType],
  fields: Seq[VariableDecl],
  characteristic: Option[Formula],
  members: Seq[MemberPredicateDecl],
  isAbstract: Boolean = false,
  isFinal: Boolean = false
) extends SourceLocation

case class MemberPredicateDecl(
  name: Name,
  params: Seq[VariableDecl],
  resultType: Option[QlType],
  body: Formula,
  isOverride: Boolean = false
) extends SourceLocation

case class SelectQuery(from: Seq[VariableDecl], where: Option[Formula], columns: Seq[SelectColumn]) extends SourceLocation

case class SelectColumn(expr: Expr, label: Option[Name]) extends SourceLocation

enum QlType extends SourceLocation:
  case IntType
  case FloatType
  case StringType
  case BooleanType
  case DateType
  case EntityType(name: Name)

  override def toString: String = this match
    case IntType => "int"
    case FloatType => "float"
    case StringType => "string"
    case BooleanType => "boolean"
    case DateType => "date"
    case EntityType(name) => name.toString

enum Literal extends SourceLocation:
  case IntValue(value: Int)
  case FloatValue(value: Double)
  case StringValue(value: String)
  case BooleanValue(value: Boolean)

enum Expr extends Typeable[QlType] with SourceLocation:
  case Var(name: Name)
  case Constant(value: Literal)
  case Wildcard
  case Call(name: Name, args: Seq[Expr])
  case MemberCall(receiver: Expr, name: Name, args: Seq[Expr])
  case Binary(lhs: Expr, op: String, rhs: Expr)
  case Unary(op: String, expr: Expr)

// TODO: Compare to https://codeql.github.com/docs/ql-language-reference/ql-language-specification/#formulas
enum Formula extends SourceLocation:
  case And(parts: Seq[Formula])
  case Or(parts: Seq[Formula])
  case Not(formula: Formula)
  case Call(name: Name, args: Seq[Expr])
  case MemberCall(receiver: Expr, name: Name, args: Seq[Expr])
  case Compare(lhs: Expr, op: String, rhs: Expr)
  case InRange(value: Expr, lower: Expr, upper: Expr)
  case Exists(vars: Seq[VariableDecl], formula: Formula)
  case Truth(value: Boolean)
