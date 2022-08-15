package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.util.Scala

import java.util.UUID

trait Expression extends SourceLocation {
  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

  lazy val nodeId: Int = UUID.randomUUID().hashCode()
  lazy val nodeName: String = this.getClass.getSimpleName

  def dotString(): String =
    s"""$nodeId [label="$nodeName", shape=circle];\n"""
}

case class FieldReadExpr(name: Name, value: Expression) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$value.$name"

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${value.nodeId};\n${value.dotString()}"
}

case class VarReadExpr(targetName: Name) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$targetName"
}

case class ConstructorExpr(className: Name, args: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"new $className($argsS)"
  }

  override def dotString(): String =
    s"${super.dotString()}" + args.zipWithIndex.map { case (arg, i) =>
      s"""$nodeId -> ${arg.nodeId} [label="arg[$i]"];\n${arg.dotString()}"""
    }.mkString("")
}

object CompareOp extends Enumeration {
  type CompareOp = Value

  val EQ: Value = Value("==")
  val LT: Value = Value("<")
  val GT: Value = Value(">")
}

import CompareOp._

case class CompareExpr(left: Expression, right: Expression, op: CompareOp) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"($left $op $right)"
  }

  override def dotString(): String =
    s"${super.dotString()}" +
      s"""$nodeId -> ${left.nodeId} [label="left"];\n${left.dotString()}""" +
      s"""$nodeId -> ${right.nodeId} [label="right"];\n${right.dotString()}""".stripMargin
}

case class MethodCallExpr(fun: Name, args: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"$fun($argsS)"
  }

  override def dotString(): String =
    s"${super.dotString()}" + args.zipWithIndex.map { case (arg, i) =>
      s"""$nodeId -> ${arg.nodeId} [label="arg[$i]"];\n${arg.dotString()}"""
    }.mkString("")
}

case class BaseLit(code: Scala[meta.Term]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = s"Base(${code.tree match {
    case meta.Lit.Int(i) => i.toString
    case meta.Lit.Long(l) => l.toString
    case meta.Lit.Float(f) => f
    case meta.Lit.Double(d) => d
    case meta.Lit.String(s) => s
    case meta.Lit.Boolean(b) => b.toString
    case meta.Lit.Char(c) => c.toString
    case t => s"`${t.syntax}`"
  }})"
}