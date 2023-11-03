package inca.frontend.oodl.syntax

import inca.ir.Name
import inca.ir.typing.Resolvable
import inca.ir.util.SourceLocation

sealed trait Statement extends SourceLocation:
  def vars: Map[Name, Option[Type]]
  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

case class Expr(expression: Expression) extends Statement:
  override def vars: Map[Name, Option[Type]] = expression.vars
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$indent$expression"

case class Return(expression: Expression) extends Statement:
  override def vars: Map[Name, Option[Type]] = expression.vars
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"${indent}return $expression"

case class Assign(lhs: Expression, rhs: Expression) extends Statement with Resolvable[Either[(ClassDef, FieldDef), Var.Target]]:
  override def vars: Map[Name, Option[Type]] = lhs.vars ++ rhs.vars
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$indent$lhs = $rhs"

case class VarDeclare(name: Name, typ: Option[Type], maybeExpression: Option[Expression], immutable: Boolean) extends Statement with Var.Target:
  override def vars: Map[Name, Option[Type]] = if (maybeExpression.isDefined) maybeExpression.get.vars else Map()
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val expr = if (maybeExpression.isEmpty) "" else s" = ${maybeExpression.get.toString}"
    val prefix = if (immutable) "val " else "var "
    s"${indent}${prefix}${name}: $typ$expr"
  }

case class If(cnd: Expression, thn: Seq[Statement], els: Seq[Statement]) extends Statement:
  override def vars: Map[Name, Option[Type]] = (thn.flatMap(_.vars) ++ els.flatMap(_.vars)).toMap
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
      val condS = s"if (${cnd.prettyprint})"
      val ifS = thn.map(_.prettyprint(indent + "\t")).mkString("\n")
      val elseS = els.map(_.prettyprint(indent + "\t")).mkString("\n")

      if (elseS.isEmpty) {
        s"${indent}$condS {\n$ifS\n$indent}"
      } else {
        s"${indent}$condS {\n$ifS\n$indent} else {\n$elseS\n$indent}"
      }
    }