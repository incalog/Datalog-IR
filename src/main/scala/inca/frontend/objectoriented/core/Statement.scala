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
    s"$indent$expression"

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${expression.nodeId};\n${expression.dotString()}"
}

case class ReturnStmt(value: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"${indent}return $value"
  }

  override def dotString(): String = {
      s"${super.dotString()}$nodeId -> ${value.nodeId};\n${value.dotString()}"
  }
}

case class FieldAssignStmt(recv: Expression, name: Name, value: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$indent$recv.$name = $value"
  }

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${value.nodeId};\n${value.dotString()}"
}

case class VarDeclareStmt(name: Name, typ: Type, value: Option[Expression]) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val expr = if (value.isEmpty) "" else s" = ${value.get.toString}"
    s"${indent}var ${name}: $typ$expr"
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
    s"$indent$targetName = $value"
  }

  override def dotString(): String =
    s"${super.dotString()}$nodeId -> ${value.nodeId};\n${value.dotString()}"
}

case class IfStmt(cnd: Expression, thn: Seq[Statement], els: Seq[Statement]) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val condS = s"if (${cnd.prettyprint})"
    val ifS = thn.map(_.prettyprint(indent+"\t")).mkString("\n")
    val elseS = els.map(_.prettyprint(indent+ "\t")).mkString("\n")

    if (elseS.isEmpty) {
      s"${indent}$condS {\n$ifS\n$indent}"
    } else {
      s"${indent}$condS {\n$ifS\n$indent} else {\n$elseS\n$indent}"
    }
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