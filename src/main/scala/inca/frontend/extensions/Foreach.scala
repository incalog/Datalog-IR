package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.typechecker.StmType
import inca.util.Gensym

case class Foreach(name: Name, exp: Expression, body: Body) extends Statement with Var.Target {
  val elemTyp: Option[Type] = exp.typ.flatMap {
    case ty: TIterable => Some(ty.contained)
    case _ => None
  }

  override def boundVars: Set[Name] = Set(name) ++ body.boundVars
  override def allVars: Map[Name, Option[Type]] = Map(name -> elemTyp) ++ exp.freeVars ++ body.allVars

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
    ).mapWithLoc { case (s, e, b) => Foreach(s, e, b) } |
      super.statement

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("foreach", "in")

  override protected def typecheckInternal(stm: Statement, mustTerminate: Boolean): StmType = stm match {
    case foreach@Foreach(name, exp, body) =>
      val ety = typecheck(exp)
      val elemType = ety match {
        case it: TIterable => it.contained
        case _ =>
          error(s"Found $ety, but expected iterable type", exp)
          TAny
      }

      scopedTypeContext {
        bindVar(name, foreach, elemType)
        typecheck(body, mustTerminate)
      }

    case _ => super.typecheckInternal(stm, mustTerminate)
  }
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
