//package inca.frontend.functional.core
//
//import inca.compiler.SourceLocation
//import inca.frontend.functional.typechecker.Typeable
//
//trait Statement extends Typeable with SourceLocation {
//  def vars: Map[Name, Option[Type]]
//  def prettyprint(implicit indent: String): String
//  override def toString: String = prettyprint("")
//
//  def ensureCore: CoreStatement = this match {
//    case self: CoreStatement => self
//    case _ => throw new IllegalArgumentException(s"Core statement required but got $this")
//  }
//}
//
//sealed trait CoreStatement extends Statement
//
//
//case class ExpStm(exp: Expression) extends CoreStatement {
//  override def vars: Map[Name, Option[Type]] = exp.vars
//  override def prettyprint(implicit indent: String): String = exp.prettyprint
//}
//
//case class ValStm(names: Seq[Name], anno: Option[Type], bound: Expression) extends CoreStatement with Var.Target {
//  override def vars: Map[Name, Option[Type]] = bound.vars ++ (bound.typ match {
//    case Some(ty) if names.size == 1 => Map(names.head -> Some(ty))
//    case Some(TTuple(ts)) if names.size == ts.size => (names zip ts.map(Some(_))).toMap
//    case _ => names.map(_ -> None).toMap
//  })
//
//  override def prettyprint(implicit indent: String): String = {
//    val namesS = names match {
//      case Nil => "()"
//      case Seq(name) => name.name
//      case names => names.mkString("(",", ",")")
//    }
//    val annoS = anno match {
//      case Some(ty) => s": ${ty.prettyprint}"
//      case None => ""
//    }
//    s"${indent}val $namesS$annoS = ${bound.prettyprint}"
//  }
//}
