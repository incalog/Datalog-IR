package inca.frontend.objectoriented.core

import inca.compiler.{SourceLocation, SourceObject}
import inca.frontend.util.Resolvable

sealed trait Statement extends SourceLocation {
  def vars: Map[Name, Option[Type]] = Map()

  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

  override def nodeShape: String = "Mcircle"
}

case class ExprStmt(expression: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$indent$expression"

  override def dotString: String =
    s"${super.dotString}$nodeId -> ${expression.nodeId};\n${expression.dotString}"
}

case class ReturnStmt(expression: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"${indent}return $expression"
  }

  override def dotString: String = {
      s"${super.dotString}$nodeId -> ${expression.nodeId};\n${expression.dotString}"
  }
}

case class FieldAssignStmt(recv: Expression, name: Name, expression: Expression) extends Statement with Resolvable[(ClassDef, FieldDef)] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$indent$recv.$name = $expression"
  }

  override def dotString: String =
    s"${super.dotString}$nodeId -> ${expression.nodeId};\n${expression.dotString}"
}

case class VarDeclareStmt(name: Name, typ: Type, maybeExpression: Option[Expression], immutable: Boolean) extends Statement
  with VarReadExpr.Target {

  override def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val expr = if (maybeExpression.isEmpty) "" else s" = ${maybeExpression.get.toString}"
    val prefix = if (immutable) "val " else "var "
    s"${indent}${prefix}${name}: $typ$expr"
  }

  override def dotString: String = {
    if (maybeExpression.isEmpty)
      super.dotString
    else
      s"${super.dotString}$nodeId -> ${maybeExpression.get.nodeId};\n${maybeExpression.get.dotString}"
  }
}

case class VarAssignStmt(targetName: Name, expression: Expression) extends Statement with Resolvable[VarReadExpr.Target] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$indent$targetName = $expression"
  }

  override def dotString: String =
    s"${super.dotString}$nodeId -> ${expression.nodeId};\n${expression.dotString}"
}

case class VarPhiAssignStmt(name: Name, typ: Type, ifStmt: IfStmt, thnName: Name, elsName: Name) extends Statement with VarReadExpr.Target {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"${indent}val $name: $typ := phi(${ifStmt.cnd})($thnName, $elsName)"
  }

  lazy val source: SourceObject = ifStmt.sourceObject

  override def dotString: String =
    s"${super.dotString}"
}

case class IfStmt(cnd: Expression, thn: Seq[Statement], els: Seq[Statement]) extends Statement {
  override def vars: Map[Name, Option[Type]] = (thn.flatMap(_.vars) ++ els.flatMap(_.vars)).toMap

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

  override def dotString: String =
    s"""${super.dotString}""" +
      s"""$nodeId -> ${cnd.nodeId} [label="cond"];\n${cnd.dotString}""" +
      thn.zipWithIndex.map {
        case (t, i) => s"""$nodeId -> ${t.nodeId} [label="then[$i]"];\n${t.dotString}"""
      }.mkString("") +
      els.zipWithIndex.map{
        case (e, i) => s"""$nodeId -> ${e.nodeId} [label="else[$i]"];\n${e.dotString}"""
      }.mkString("")
}