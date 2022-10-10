package inca.frontend.objectoriented.core

import inca.compiler.SourceLocation
import inca.frontend.util.{Resolvable, Typeable}
import inca.util.Scala

sealed trait Expression extends Typeable[Type] with SourceLocation {
  def prettyprint(infixParens: Boolean)(implicit indent: String): String
  def prettyprint(implicit indent: String): String = prettyprint(infixParens = false)(indent)
  override def toString: String = prettyprint("")

  def infix(infixParens: Boolean)(f: => String): String =
    if (infixParens)
      s"($f)"
    else
      f
}

case class FieldReadExpr(recv: Expression, targetName: Name) extends Expression with Resolvable[(ClassDef, FieldDef)] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$recv.$targetName"
}

case class VarReadExpr(targetName: Name) extends Expression with Resolvable[VarReadExpr.Target] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$targetName"
}
object VarReadExpr {
  trait Target extends SourceLocation
}

case class ConstructorExpr(classRef: ClassRef, args: Seq[Expression]) extends Expression with Resolvable[ConstructorDef] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"new $classRef($argsS)"
  }
}

case class SuperExpr(args: Seq[Expression]) extends Expression with Resolvable[(ClassDef, ConstructorDef)] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"this($argsS)"
  }
}

case class MethodCallExpr(recv: Expression, fun: Name, args: Seq[Expression]) extends Expression with Resolvable[MethodDef] {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    s"$recv.$fun($argsS)"
  }
}

case class TypeCastExpr(recv: Expression, toTyp: Type) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"cast($recv, $toTyp)"
}

case class InstanceOfExpr(recv: Expression, ofTyp: Type) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"instanceOf($recv, $ofTyp)"
}

case class EqualsExpr(obj1: Expression, obj2: Expression) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"eqauls($obj1, $obj2)"
}

case class NullExpr() extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"null"
}

case class TupleReadExpr(recv: Expression, index: Index) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    s"$recv._$index"
}

case class TupleExpr(exps: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    exps.map(_.prettyprint).mkString("(", ", ", ")")
}
object TupleExpr {
  def apply(): TupleExpr= TupleExpr(Seq())

  def from(exps: Seq[Expression]): Expression = exps match {
    case Nil => TupleExpr(Seq())
    case e :: Nil => e
    case es => TupleExpr(es)
  }
}

case class SetExpr(exps: Seq[Expression]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String =
    exps.map(_.prettyprint).mkString("[", ", ", "]")
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
}

case class BaseApplyInfixExpr(left: Expression, op: Scala[meta.Term.Name], right: Expression) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = infix(infixParens) {
    s"${left.prettyprint(infixParens = true)} $op ${right.prettyprint(infixParens = true)}"
  }
}

case class BaseApplyMethodExpr(recv: Expression, method: Name, args: Option[Seq[Expression]]) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    val argsS = if (args.isEmpty) "" else args.get.map(_.prettyprint).mkString(", ")
    s"${recv.prettyprint(infixParens)}.`$method`($argsS)"
  }
}

case class BaseApplyUnaryExpr(op: Scala[meta.Term.Name], exp: Expression) extends Expression {
  override def prettyprint(infixParens: Boolean)(implicit indent: String): String = {
    s"${op.syntax}${exp.prettyprint(infixParens = false)}"
  }
}
