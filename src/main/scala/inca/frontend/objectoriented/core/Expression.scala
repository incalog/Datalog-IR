package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation

trait Expression extends SourceLocation {
  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")
}

case class FieldExpr(name: Name, value: Expression) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$value.$name"
    //s"FieldReadExpr($name, $value)"
}

case class VarExpr(targetName: Name) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$targetName"
}

case class ConstructorExpr(className: Name, args: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"new $className($argsS)"
  }
}

case class MethodCallExpr(fun: Name, args: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"$fun($argsS)"
  }
}