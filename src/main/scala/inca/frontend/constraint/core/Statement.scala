package inca.frontend.constraint.core

import inca.compiler.source.SourceLocation


trait Statement extends SourceLocation {
  def boundVars: Set[Name]
  def allVars: Map[Name, Option[Type]]
  def prettyprint(implicit indent: String): String
  def ensureCore: CoreStatement = this match {
    case self: CoreStatement => self
    case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
  }
}
sealed trait CoreStatement extends Statement
case class Values(name: Name, typ: Type) extends CoreStatement with Var.Target {
  override def boundVars: Set[Name] = Set(name)
  override def allVars: Map[Name, Option[Type]] = Map(name -> Some(typ))
  override def prettyprint(implicit indent: String): String =
    s"${indent}vals $name <- ${typ.prettyprint}"
}
case class Assign(names: Seq[Name], exp: Expression) extends CoreStatement with Var.Target {
  override def boundVars: Set[Name] = names.toSet
  override def allVars: Map[Name, Option[Type]] = exp.freeVars ++ (exp.typ match {
    case Some(ty) if names.size == 1 => Map(names.head -> Some(ty))
    case Some(TTuple(ts)) if names.size == ts.size => (names zip ts.map(Some(_))).toMap
    case _ => names.map(_ -> None).toMap
  })

  override def prettyprint(implicit indent: String): String = {
    val namesS = if (names.size == 1) names.head else names.mkString("(", ", ", ")")
    s"${indent}val $namesS = ${exp.prettyprint}"
  }
}
case class Assert(cond: Expression) extends CoreStatement {
  override def boundVars: Set[Name] = Set()
  override def allVars: Map[Name, Option[Type]] = cond.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${indent}assert ${cond.prettyprint}"
}

trait TerminatorStatement extends Statement
case class Yield(exp: Expression) extends CoreStatement with TerminatorStatement {
  override def boundVars: Set[Name] = Set()
  override def allVars: Map[Name, Option[Type]] = exp.freeVars
  override def prettyprint(implicit indent: String): String =
    s"${indent}yield ${exp.prettyprint}"
}
case object FailStatement extends CoreStatement with TerminatorStatement {
  override def boundVars: Set[Name] = Set()
  override def allVars: Map[Name, Option[Type]] = Map()
  override def prettyprint(implicit indent: String): String =
    s"${indent}fail"
}
