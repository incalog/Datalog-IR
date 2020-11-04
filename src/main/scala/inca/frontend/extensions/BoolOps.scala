package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core.{Expression, _}
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.typechecker.TypeOps
import inca.util.Gensym
import inca.util.Meta.Scala

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.meta.XtensionQuasiquoteTerm

case class Not(cond: Expression) extends Expression {
  override def freeVars: Map[Name, Option[Type]] = cond.freeVars
  override def prettyprint(implicit indent: String): String = s"!(${cond.prettyprint})"
}
case class And(e1: Expression, e2: Expression) extends Expression {
  override def freeVars: Map[Name, Option[Type]] = e1.freeVars ++ e2.freeVars
  override def prettyprint(implicit indent: String): String = s"(${e1.prettyprint} && ${e2.prettyprint})"
}
case class Or(e1: Expression, e2: Expression) extends Expression {
  override def freeVars: Map[Name, Option[Type]] = e1.freeVars ++ e2.freeVars
  override def prettyprint(implicit indent: String): String = s"(${e1.prettyprint} || ${e2.prettyprint})"
}

/**
 * Extension adding boolean expressions to CoreParser.
 */
trait BoolOpsFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = BoolOps +: super.desugarables

  override protected[frontend] def infixExp[_: P]: P[Expression] = Chain(andExp, "||", andExp, Or)
  protected[frontend] def andExp[_: P]: P[Expression] = Chain(notExp, "&&", notExp, And)
  protected[frontend] def notExp[_: P]: P[Expression] = P(("!" ~ super.infixExp).mapWithLoc(Not) | super.infixExp)

  override def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case Not(cond) =>
      val ty = typecheck(cond)
      if (!TypeOps.subtype(ty, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty", cond)
      TScalaBoolean
    case And(e1, e2) =>
      val ty1 = typecheck(e1)
      val ty2 = typecheck(e2)
      if (!TypeOps.subtype(ty1, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty1", e1)
      if (!TypeOps.subtype(ty2, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty2", e2)
      TScalaBoolean
    case Or(e1, e2) =>
      val ty1 = typecheck(e1)
      val ty2 = typecheck(e2)
      if (!TypeOps.subtype(ty1, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty1", e1)
      if (!TypeOps.subtype(ty2, TScalaBoolean, lang))
        error(s"Expected Boolean expression, but got $ty2", e2)
      TScalaBoolean
    case _ => super.typecheckInternal(exp, anno)
  }
}

object BoolOps extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(Switch)

  override def trans(): DesugarTrans = new DesugarTrans {
    val boolStatements: ListBuffer[Statement] = ListBuffer()
    val orAlternatives: mutable.MultiDict[Name, Expression] = mutable.MultiDict()

    override def desugarExp(cond: Expression)(implicit gensym: Gensym): Expression = cond match {
      case Not(cond) =>
        desugarNot(cond).orTyped(TScalaBoolean)
      case And(e1, e2) =>
        boolStatements += Assert(desugarExp(e1).orTyped(TScalaBoolean))
        changed(desugarExp(e2).orTyped(TScalaBoolean))
      case Or(e1, e2) =>
        val sym = Name(gensym.fresh("or"))
        orAlternatives += sym -> e1.orTyped(TScalaBoolean)
        orAlternatives += sym -> e2.orTyped(TScalaBoolean)
        changed(Var(sym))
      case _ => super.desugarExp(cond)
    }

    def desugarNot(cond: Expression)(implicit gensym: Gensym): Expression = cond match {
      case Not(cond) => changed(desugarExp(cond))
      case And(e1, e2) => changed(desugarExp(Or(Not(e1), Not(e2))))
      case Or(e1, e2) => changed(desugarExp(And(Not(e1), Not(e2))))

      case Eq(lhs, rhs) => changed(Neq(desugarExp(lhs), desugarExp(rhs)))
      case Neq(lhs, rhs) => changed(Eq(desugarExp(lhs), desugarExp(rhs)))
      case InstanceOf(exp, typ) => changed(NotInstanceOf(desugarExp(exp), typ))
      case NotInstanceOf(exp, typ) => changed(InstanceOf(desugarExp(exp), typ))
      case Def(exp) => changed(Undef(desugarExp(exp)))
      case Undef(exp) => changed(Def(desugarExp(exp)))
      case Constant(BooleanLiteral(v)) => changed(Constant(BooleanLiteral(!v)))
      case Eval(vars, code) => changed(Eval(vars, Scala(q"!{${code.tree}}")))

      case _ => Not(cond)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      val stms =
        if (boolStatements.isEmpty)
          desugared
        else {
          val prepend = boolStatements.toSeq
          boolStatements.clear()
          prepend ++ desugared
        }
      if (orAlternatives.isEmpty)
        stms
      else {
        val alts = orAlternatives.toSeq.map { case (sym, exp) =>
          Body(Assign(Seq(sym), exp) +: stms)
        }
        orAlternatives.clear()
        Seq(Switch(alts))
      }
    }
  }
}

