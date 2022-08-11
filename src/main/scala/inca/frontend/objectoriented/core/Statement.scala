package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation

trait Statement extends SourceLocation {
  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")
}

case class ExprStmt(expression: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$expression"
}

case class ReturnStmt(expression: Option[Expression]) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val exp = if (expression.isEmpty) "" else expression.get.toString
    s"return $exp"
  }
}

case class FieldAssignStmt(target: FieldExpr, value: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$target = $value"
  }
}

case class VarAssignStmt(target: VarExpr, value: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$target = $value"
  }
}