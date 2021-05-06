package inca.frontend.constraint.core

import inca.compiler.SourceLocation
import inca.frontend.util.{Resolvable, Typeable}
import inca.util.Meta.Scala

trait Expression extends Typeable[Type] with SourceLocation {
  def freeVars: Map[Name, Option[Type]]
  def prettyprint(implicit indent: String): String
  override def toString: String = prettyprint("")

  def ensureCore: CoreExpression = this match {
    case self: CoreExpression => self
    case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
  }
}

sealed trait CoreExpression extends Expression

case class Var(name: Name) extends CoreExpression with Resolvable[Var.Target] {
  override def freeVars: Map[Name, Option[Type]] = Map(name -> typ)
  override def prettyprint(implicit indent: String): String = name.name
}
object Var {
  def apply(name: String): Var = new Var(Name(name))
  trait Target extends SourceLocation
}

case class Eq(lhs: Expression, rhs: Expression) extends CoreExpression {
  def freeVars: Map[Name, Option[Type]] = lhs.freeVars ++ rhs.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${lhs.prettyprint} == ${rhs.prettyprint}"
}
case class Neq(lhs: Expression, rhs: Expression) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = lhs.freeVars ++ rhs.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${lhs.prettyprint} != ${rhs.prettyprint}"
}
case class InstanceOf(exp: Expression, ty: Type) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = exp.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${exp.prettyprint}.isInstanceOf[${ty.prettyprint}]"
}
case class NotInstanceOf(exp: Expression, ty: Type) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = exp.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${exp.prettyprint}.notInstanceOf[${ty.prettyprint}]"
}
case class Cast(src: Expression, targetTyp: Type) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = src.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${src.prettyprint}:${targetTyp.prettyprint}"
}
case class Def(exp: Expression) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = exp.freeVars
  override def prettyprint(implicit indent: String): String =
    s"def ${exp.prettyprint}"
}
case class Undef(exp: Expression) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = exp.freeVars
  override def prettyprint(implicit indent: String): String =
    s"undef ${exp.prettyprint}"
}

case object Wildcard extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = Map()
  override def prettyprint(implicit indent: String): String = "_"
}
case class Constant(lit: Literal) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = Map()
  override def prettyprint(implicit indent: String): String = lit.prettyprint
}
case class PathAccess(receiver: Expression, link: Link) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = receiver.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${receiver.prettyprint}.${link.prettyprint}"
}

case class Call(name: Name, args: Seq[Expression], transitive: Boolean = false)
    extends CoreExpression with Resolvable[Call.Target] {
  override def freeVars: Map[Name, Option[Type]] = args.flatMap(_.freeVars).toMap
  override def prettyprint(implicit indent: String): String = {
    val argsS = args.map(_.prettyprint).mkString(", ")
    val transS = if (transitive) "+" else ""
    s"$name$transS($argsS)"
  }
}
object Call {
  trait Target
}

case class Count(call: Call) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = call.freeVars
  override def prettyprint(implicit indent: String): String = s"count ${call.prettyprint}"
}
case class Tuple(exps: Seq[Expression]) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = exps.flatMap(_.freeVars).toMap

  override def prettyprint(implicit indent: String): String =
    exps.map(_.prettyprint).mkString("(", ", ", ")")
}
/** Eval code must be a Scala expression that can access `params` by name and must yield a `resultType`. */
case class Eval(code: Scala[meta.Term]) extends CoreExpression {
  var params: Option[Seq[EvalParam]] = None

  override def freeVars: Map[Name, Option[Type]] = params match {
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

case class EvalParam(name: Name) extends SourceLocation with Typeable[Type] with Resolvable[Var.Target] {
  override def toString: String = name.toString
}

case class Aggregate(agg: Expression, bodies: Seq[Body]) extends CoreExpression {
  override def freeVars: Map[Name, Option[Type]] = agg.freeVars ++ bodies.flatMap(_.freeVars)

  override def prettyprint(implicit indent: String): String = {
    val bodiesS = if (bodies.isEmpty) "{ }" else
      bodies.map(_.prettyprint).mkString(" union ")
    s"aggregate($agg) $bodiesS"
  }
}
object Aggregate {
  def apply(agg: Expression, call: Call): Aggregate = new Aggregate(agg, Seq(Body(Yield(call))))
}
