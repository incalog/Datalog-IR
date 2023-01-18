package inca.frontend.objectoriented.core

import inca.compiler.{SourceLocation, SourceObject}
import inca.frontend.util.Resolvable


case class AssignmentOp(raw: String) extends SourceLocation {
  lazy val isAggregation: Boolean = raw.startsWith("+")
  override def toString: String = raw
}

object AssignmentOp {
  val EQUAL: AssignmentOp       = AssignmentOp("=")
  val AGG: AssignmentOp         = AssignmentOp("++=")
  val AGG_ELEMENT: AssignmentOp = AssignmentOp("+=")

  lazy val values: Seq[AssignmentOp] = Seq(EQUAL, AGG, AGG_ELEMENT)
  def from(raw: String): Option[AssignmentOp] = raw match {
    case EQUAL.raw => Some(EQUAL)
    case AGG.raw => Some(AGG)
    case AGG_ELEMENT.raw => Some(AGG_ELEMENT)
    case _ => None
  }
}

sealed trait Statement extends SourceLocation {
  def vars: Map[Name, Option[Type]] = Map()

  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")
}

case class ExprStmt(expression: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$indent$expression"
}

case class ReturnStmt(expression: Expression) extends Statement {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"${indent}return $expression"
}

case class FieldAssignStmt(recv: Expression, name: Name, expression: Expression) extends Statement
  with Resolvable[(ClassDef, FieldDef)] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"$indent$recv.$name = $expression"
  }
}

case class VarDeclareStmt(name: Name, typ: Type, maybeExpression: Option[Expression], immutable: Boolean) extends Statement
  with VarReadExpr.Target {

  override def vars: Map[Name, Option[Type]] = Map(name -> Some(typ))

  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val expr = if (maybeExpression.isEmpty) "" else s" = ${maybeExpression.get.toString}"
    val prefix = if (immutable) "val " else "var "
    s"${indent}${prefix}${name}: $typ$expr"
  }
}

case class VarAssignStmt(targetName: Name, expression: Expression) extends Statement with Resolvable[VarReadExpr.Target] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$indent$targetName = $expression"
}

case class VarPhiAssignStmt(name: Name, typ: Type, ifStmt: IfStmt, thnName: Name, elsName: Name) extends Statement with VarReadExpr.Target {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"${indent}val $name: $typ := phi(${ifStmt.cnd})($thnName, $elsName)"

  lazy val source: SourceObject = ifStmt.sourceObject
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
}