package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

case class Foreach(name: Name, exp: Exp, body: Body) extends Statement {
  val elemTyp: Option[TLinked] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }

  override def boundVars: Set[Name] = Set(name) ++ body.boundVars
  override def allVars: Map[Name, Option[TypeAnno]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

  override def prettyprint(implicit indent: String): String = {
    s"${indent}foreach $name in ${exp.prettyprint} ${body.prettyprint}"
  }
}

/**
 * Extension adding "forallexists" statements to @see Parser.
 */
trait ForeachFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = Foreach +: super.desugarables

  override protected[frontend] def statement[_: P]: P[Statement] =
    P(
      "foreach " ~ identifier ~~ " " ~ "in " ~ exp ~ body
    ).map { case (s, e, b) => Foreach(s, e, b) } |
      super.statement

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("foreach", "in")
}

object Foreach extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Foreach(name, exp, body) => exp.typ match {
        case Some(TList(ty)) => changed(Assign(Seq(name), PathAccess(exp, ChildrenLink).typed(ty)) +: body.stmts.flatMap(desugarStm))
        case Some(TEnumeration(ty)) => changed(Assign(Seq(name), exp.orTyped(ty)) +: body.stmts.flatMap(desugarStm))
        case Some(ty) => throw new IllegalArgumentException(s"Foreach loop expression $exp must have iterable type, but was type $ty")
        case None => throw new IllegalArgumentException(s"Cannot support foreach loop with untyped expression $exp")
      }

      case _ => super.desugarStm(stm)
    }
  }
}
