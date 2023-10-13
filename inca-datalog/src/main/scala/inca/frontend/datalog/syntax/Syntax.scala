package inca.frontend.datalog.syntax

import inca.ir.Name
import inca.ir.typing.Typeable
import inca.ir.util.SourceLocation

case class Module(relations: Seq[Relation]) extends SourceLocation {
  override def toString: String = relations.mkString("\n")
}

case class Relation(name: Name, params: Seq[Type], rules: Seq[Rule]) extends SourceLocation:
  override def toString: String =
    val sig = s"$name(${params.mkString(",")})."
    val rs = rules.map(_.toString(name)).mkString("\n")
    sig + "\n" + rs + "\n"

case class Rule(head: Seq[Param], body: Seq[Atom]) extends SourceLocation:
  def toString(name: Name): String =
    if (body.isEmpty)
      s"$name(${head.mkString(",")})."
    else
      s"$name(${head.mkString(",")}) :- ${body.mkString(", ")}."

enum Param extends SourceLocation:
  case Named(name: Name)
  case Aggregated(name: Name, agg: Name)
  case Constant(lit: Literal)

  override def toString: String = this match
    case Named(name) => name.toString
    case Aggregated(name, agg) => s"$agg($name)"
    case Constant(lit) => lit.toString

enum Type extends SourceLocation:
  case Int()
  case Double()
  case String()

  override def toString: Predef.String = this match
    case Int() => "Int"
    case Double() => "Double"
    case String() => "String"

enum Atom extends SourceLocation:
  case Call(name: Name, args: Seq[Term], not: Boolean)
  case Compare(lhs: Term, op: String, rhs: Term)

  override def toString: String = this match
    case Call(name, args, not) =>
      (if (not)
        "not "
      else
        "") + s"$name(${args.mkString(",")})"
    case Compare(lhs, op, rhs) => s"$lhs $op $rhs"

enum Term extends Typeable[Type] with SourceLocation:
  case Var(name: Name)
  case Constant(lit: Literal)
  case BinOp(lhs: Term, op: String, rhs: Term)

  override def toString: String = this match
    case Var(name) => name.toString
    case Constant(lit) => lit.toString
    case BinOp(lhs, op, rhs) => s"($lhs $op $rhs)"

enum Literal extends Typeable[Type] with SourceLocation:
  case Int(i: scala.Int)
  case Double(d: scala.Double)
  case String(s: Predef.String)

  override def toString: Predef.String = this match
    case Int(i) => i.toString
    case Double(d) => d.toString
    case String(s) => s
