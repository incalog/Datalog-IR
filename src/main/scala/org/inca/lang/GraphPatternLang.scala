package org.inca.lang

import org.inca.meta.MetaElements.{Link, NodeType}

object GraphPatternLang {
  trait Type
  case class TNodeType(wrapped: NodeType) extends Type
  case object TBool extends Type
  case object TInt extends Type
  case object TLong extends Type
  case object TDouble extends Type
  case object TString extends Type

  implicit def nodeTypeToType(t: NodeType): Type = TNodeType(t)

  type Name = String

  case class Module(name: Name, imports: Seq[Name], pats: Seq[GraphPattern])
  case class GraphPattern(vis: Option[Visibility], name: Name, params: Seq[Param], bodies: Seq[Alternative])

  sealed trait Visibility
  case object Private extends Visibility
  case object Public extends Visibility

  case class Param(name: Name, typ: Option[Type])

  // Body
  case class Alternative(constraints: Seq[Constraint])

  // would be atom
  sealed trait Constraint
  // would be Relation, maybe remove neg arguement and create negation atom
  case class Composition(call: PatternCall, neg: Boolean) extends Constraint
  case class Compare(comp: Comparator, lhs: Value, rhs: Value) extends Constraint
  // these concepts are inca specific to query AST information
  // TODO Value => Var
  case class Concept(v: Value, typ: Type) extends Constraint
  // Value => Var
  case class Path(src: Value, trg: Value, link: Link, typ: Type) extends Constraint
  // TODO how do we represent java code?
  case class Check(code: String) extends Constraint

  case class PatternCall(name: Name, args: Seq[Value], transitive: Boolean)

  sealed trait Comparator
  case object EqComparator extends Comparator
  case object NeqComparator extends Comparator

  // This would be term
  sealed trait Value
  case class Var(name: Name) extends Value
  // Would be constant
  case class Constant(lit: Literal) extends Value

  sealed trait Literal
  case class IntLiteral(v: Int) extends Literal
  case class LongLiteral(v: Long) extends Literal
  case class DoubleLiteral(v: Double) extends Literal
  case class StringLiteral(v: String) extends Literal
  case class BooleanLiteral(v: Boolean) extends Literal
}
