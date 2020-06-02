package org.inca.lang

import org.inca.meta.MetaElements.{Link, NodeType}

object FunLang {
  type Type = NodeType
  type Name = String

  case class Module(name: Name, imports: Seq[Name], funs: Seq[PatternFunction])
  case class PatternFunction(vis: Option[Visibility], name: Name, params: Seq[Param], outParams: Seq[AnnoParam], bodies: Seq[Alternative])

  sealed trait Visibility
  case object Private extends Visibility
  case object Public extends Visibility

  case class Param(name: Name, typ: Option[Type])
  case class AnnoParam(name: Name, typ: Type)

  case class Alternative(stmts: Seq[Statement])

  sealed trait Statement
  case class Assignment(names: Seq[Name], exp: Exp) extends Statement
  case class Assert(cond: Cond) extends Statement
  case class Return(exp: Exp) extends Statement
  // TODO these are extensions
  case class Switch(alts: Seq[Alternative]) extends Statement
  case class StatementList(seq: Seq[Statement]) extends Statement

  sealed trait Cond
  case class Eq(lhs: Exp, rhs: Exp) extends Cond
  case class Neq(lhs: Exp, rhs: Exp) extends Cond
  case class InstanceOf(exp: Exp, typ: Type) extends Cond
  case class NotInstanceOf(exp: Exp, typ: Type) extends Cond
  case class Def(exp: Exp) extends Cond
  case class Undef(exp: Exp) extends Cond

  sealed trait Exp
  case class Var(name: Name) extends Exp
  case class Constant(lit: Literal) extends Exp
  // TODO do not allow nested pathaccesses (preprocess flatten)
  case class PathAccess(exp: Exp, path: Seq[Link]) extends Exp
  case class Call(call: PatternCall, count: Boolean) extends Exp
  case class Tuple(exps: Seq[Exp]) extends Exp

  case class PatternCall(name: Name, args: Seq[Exp], transitive: Boolean)

  sealed trait Literal
  case class IntLiteral(v: Int) extends Literal
  case class FloatLiteral(v: Float) extends Literal
  case class DoubleLiteral(v: Double) extends Literal
  case class StringLiteral(v: String) extends Literal
  case class BooleanLiteral(v: Boolean) extends Literal
}
