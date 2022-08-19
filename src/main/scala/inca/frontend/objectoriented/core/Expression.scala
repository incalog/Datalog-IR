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

case class FieldReadExpr(recv: Expression, targetName: Name) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$recv.$targetName"

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

case class MethodCallExpr(recv: Expression, fun: Name, args: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"$recv.$fun($argsS)"
  }

  override def dotString(): String =
    s"${super.dotString()}" + args.zipWithIndex.map { case (arg, i) =>
      s"""$nodeId -> ${arg.nodeId} [label="arg[$i]"];\n${arg.dotString()}"""
    }.mkString("")
}

case class TypeCastExpr(recv: Expression, typ: Type) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"cast($recv, $typ)"

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${recv.nodeId};\n${recv.dotString()}"
}

// TODO: Support syntax to create a tuple
case class TupleExpr(exps: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    exps.map(_.prettyprint).mkString("(", ", ", ")")

  override def dotString(): String =
    s"${super.dotString()}" + exps.zipWithIndex.map { case (exp, i) =>
      s"""$nodeId -> ${exp.nodeId} [label="expr[$i]"];\n${exp.dotString()}"""
    }.mkString("")
}
object TupleExpr {
  def apply(): TupleExpr= TupleExpr(Seq())

  def from(exps: Seq[Expression]): Expression = exps match {
    case Nil => TupleExpr(Seq())
    case e :: Nil => e
    case es => TupleExpr(es)
  }
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
    s"${op.syntax}${exp.prettyprint(infixParens = false)}"
  }

  override def dotString(): String =
    s"${super.dotString()}" +
      s"""$nodeId -> ${exp.nodeId} [label="exp"];\n${exp.dotString()}"""
}
