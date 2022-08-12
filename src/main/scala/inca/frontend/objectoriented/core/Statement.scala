package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation

import java.util.UUID

trait Statement extends SourceLocation {
  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

  lazy val nodeId: Int = UUID.randomUUID().hashCode()
  lazy val nodeName: String = this.getClass.getSimpleName

  def dotString(): String =
    s"""$nodeId [label="$nodeName", shape=Mcircle];\n"""
}

case class ExprStmt(expression: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$expression"

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${expression.nodeId};\n${expression.dotString()}"
}

case class ReturnStmt(value: Option[Expression]) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val expr = if (value.isEmpty) "" else value.get.toString
    s"return $expr"
  }

  override def dotString(): String = {
    if (value.isEmpty)
      super.dotString()
    else
      s"${super.dotString()}$nodeId -> ${value.get.nodeId};\n${value.get.dotString()}"
  }
}

case class FieldAssignStmt(target: Expression, value: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$target = $value"
  }

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${value.nodeId};\n${value.dotString()}"
}

case class VarDeclareStmt(name: Name, typ: Type, value: Option[Expression]) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val expr = if (value.isEmpty) "" else s" = ${value.get.toString}"
    s"var ${name}: $typ$expr"
  }

  override def dotString(): String = {
    if (value.isEmpty)
      super.dotString()
    else
      s"${super.dotString()}$nodeId -> ${value.get.nodeId};\n${value.get.dotString()}"
  }
}

case class VarAssignStmt(targetName: Name, value: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$targetName = $value"
  }

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${value.nodeId};\n${value.dotString()}"
}

case class IfStmt(cnd: Expression, thn: Seq[Statement], els: Seq[Statement]) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"""if (${cnd.prettyprint})
       |${indent}${thn.map(_.prettyprint(indent + "\t"))}
       |${indent}else
       |${indent}${els.map(_.prettyprint(indent + "\t"))}""".stripMargin
  }

  override def dotString(): String =
    s"""${super.dotString()}""" +
      s"""$nodeId -> ${cnd.nodeId} [label="cond"];\n${cnd.dotString()}""" +
      thn.zipWithIndex.map {
        case (t, i) => s"""$nodeId -> ${t.nodeId} [label="then[$i]"];\n${t.dotString()}"""
      }.mkString("") +
      els.zipWithIndex.map{
        case (e, i) => s"""$nodeId -> ${e.nodeId} [label="else[$i]"];\n${e.dotString()}"""
      }.mkString("")
}