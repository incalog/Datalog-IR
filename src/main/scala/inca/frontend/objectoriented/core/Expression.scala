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

  def infix(infixParens: Boolean)(f: => String): String =
    if (infixParens)
      s"($f)"
    else
      f
}

case class FieldReadExpr(recv: Expression, name: Name) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$recv.$name"

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${recv.nodeId};\n${recv.dotString()}"
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

case class BaseLitExpr(code: Scala[meta.Term]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = code.tree match {
    case meta.Lit.Int(i) => i.toString
    case meta.Lit.Long(l) => l.toString
    case meta.Lit.Float(f) => f
    case meta.Lit.Double(d) => d
    case meta.Lit.String(s) => s
    case meta.Lit.Boolean(b) => b.toString
    case meta.Lit.Char(c) => c.toString
    case t => s"`${t.syntax}`"
  }
}

case class BaseApplyExpr(fun: Scala[meta.Term], args: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"`$fun`($argsS)"
  }

  override def dotString(): String =
    s"${super.dotString()}" + args.zipWithIndex.map { case (arg, i) =>
      s"""$nodeId -> ${arg.nodeId} [label="arg[$i]"];\n${arg.dotString()}"""
    }.mkString("")
}

case class BaseApplyInfixExpr(left: Expression, op: Scala[meta.Term.Name], right: Expression) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    s"${left.prettyprint(infixParens = true)} $op ${right.prettyprint(infixParens = true)}"
  }

  override def dotString(): String =
    s"${super.dotString()}" +
      s"""$nodeId -> ${left.nodeId} [label="left"];\n${left.dotString()}""" +
      s"""$nodeId -> ${right.nodeId} [label="right"];\n${right.dotString()}""".stripMargin
}

case class BaseApplyMethodExpr(recv: Expression, method: Name, args: Option[Seq[Expression]]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = if (args.isEmpty) "" else args.get.map(_.prettyprint).mkString(", ")
    s"${recv.prettyprint(infixParens)}.`$method`($argsS)"
  }

  override def dotString(): String = {
    val recvS = s"""$nodeId -> ${recv.nodeId} [label="recv"];\n${recv.dotString()}"""
    val argsS = if (args.isEmpty) "" else args.get.zipWithIndex.map { case (arg, i) =>
      s"""$nodeId -> ${arg.nodeId} [label="arg[$i]"];\n${arg.dotString()}"""
    }.mkString("")
    s"${super.dotString()}$recvS$argsS"
  }
}

case class BaseApplyUnaryExpr(op: Scala[meta.Term.Name], exp: Expression) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$indent${op.syntax}${exp.prettyprint(false)}"
  }

  override def dotString(): String =
    s"${super.dotString()}" +
      s"""$nodeId -> ${exp.nodeId} [label="exp"];\n${exp.dotString()}"""
}
