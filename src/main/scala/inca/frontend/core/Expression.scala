package inca.frontend.core

import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.{Resolvable, Typeable}
import inca.util.Meta.Scala

trait Expression extends Typeable with SourceLocation {
  def vars: Map[Name, Option[Type]]
  def prettyprint(implicit indent: String): String
  override def toString: String = prettyprint("")

  def ensureCore: CoreExpression = this match {
    case self: CoreExpression => self
    case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
  }
}

sealed trait CoreExpression extends Expression

case class Var(name: Name) extends CoreExpression with Resolvable[Var.Target] {
  override def vars: Map[Name, Option[Type]] = Map(name -> typ)
  override def prettyprint(implicit indent: String): String = name.name
}
object Var {
  def apply(name: String): Var = new Var(Name(name))
  trait Target extends SourceLocation
}

case class Let(names: Seq[Name], anno: Option[Type], bound: Expression, body: Expression) extends CoreExpression with Var.Target {
  override def vars: Map[Name, Option[Type]] = bound.vars ++ body.vars ++ (bound.typ match {
    case Some(ty) if names.size == 1 => Map(names.head -> Some(ty))
    case Some(TTuple(ts)) if names.size == ts.size => (names zip ts.map(Some(_))).toMap
    case _ => names.map(_ -> None).toMap
  })

  override def prettyprint(implicit indent: String): String = {
    val namesS = names match {
      case Nil => "()"
      case Seq(name) => name.name
      case names => names.mkString("(",", ",")")
    }
    val annoS = anno match {
      case Some(ty) => s": ${ty.prettyprint}"
      case None => ""
    }
    s"""let $namesS$annoS = ${bound.prettyprint} in
       |${indent}  ${body.prettyprint(indent + "  ")}""".stripMargin
  }
}

case class If(cnd: Expression, thn: Expression, els: Expression) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = cnd.vars ++ thn.vars ++ els.vars

  override def prettyprint(implicit indent: String): String =
    s"""if (${cnd.prettyprint}) {
       |$indent  ${thn.prettyprint(indent + "  ")}
       |$indent} else {
       |$indent  ${els.prettyprint(indent + "  ")}
       |$indent}
       |""".stripMargin
}

case class Call(name: Name, args: Seq[Expression], transitive: Boolean = false)
  extends CoreExpression with Resolvable[Call.Target] {
  override def vars: Map[Name, Option[Type]] = args.flatMap(_.vars).toMap
  override def prettyprint(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    val transS = if (transitive) "+" else ""
    s"$name$transS($argsS)"
  }
}
object Call {
  trait Target
}

case class Tuple(exps: Seq[Expression]) extends CoreExpression {
  override def vars: Map[Name, Option[Type]] = exps.flatMap(_.vars).toMap

  override def prettyprint(implicit indent: String): String =
    exps.map(_.prettyprint).mkString("(", ", ", ")")
}


case class Eval(code: Scala[meta.Term]) extends CoreExpression {
  var params: Option[Seq[EvalParam]] = None

  override def vars: Map[Name, Option[Type]] = params match {
    case Some(ps) => ps.map(p => p.name -> p.typ).toMap
    case None => Map()
  }
  override def prettyprint(implicit indent: String): String = s"`$code`"
}

object Eval {
  def apply(params: Seq[EvalParam], code: Scala[meta.Term]): Eval = {
    val eval = new Eval(code)
    eval.params = Some(params)
    eval
  }
}
case class EvalParam(name: Name) extends SourceLocation with Typeable with Resolvable[Var.Target] {
  override def toString: String = name.toString
}