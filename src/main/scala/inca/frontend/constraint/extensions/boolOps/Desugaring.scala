package inca.frontend.constraint.extensions.boolOps

import inca.frontend.constraint.core._
import inca.frontend.constraint.desugar.{DesugarTrans, Desugarable}
import inca.frontend.constraint.extensions.boolOps.Trees._
import inca.frontend.constraint.extensions.switch_
import inca.util.{Gensym, Scala}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Desugaring extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(switch_.Desugaring)
  import switch_.Trees._

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
      case eval@Eval(code) =>
        import scala.meta.quasiquotes._
        val desugaredEval = Eval(Scala(q"!{${code.tree}}"))
        desugaredEval.params = eval.params
        changed(desugaredEval)
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