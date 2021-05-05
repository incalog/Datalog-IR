package inca.frontend.constraint.extensions.forallExists

import inca.frontend.constraint.core._
import inca.frontend.constraint.desugar.{DesugarTrans, Desugarable}
import inca.frontend.constraint.extensions.forallExists.Trees._
import inca.frontend.constraint.extensions.foreach
import inca.util.Gensym
import inca.util.Meta.Scala

import scala.collection.mutable.ListBuffer
import scala.meta.Term

object Desugaring extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(foreach.Desugaring)
  import foreach.Trees._

  override def trans(): DesugarTrans = new DesugarTrans {
    val forallExistsFuns: ListBuffer[PatternFunction] = ListBuffer()

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Forall(name, exp, body) => exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot support forall over untyped expression $exp")) match {
        case TList(ty) =>
          val funsym = Name(gensym.fresh("forallCond"))
          val sizeSym = Name(gensym.fresh("listSize"))
          val successSym = Name(gensym.fresh("successSize"))

          val vars = makeCondFun(name, exp, body, ty, funsym)
          val args = vars.map(v => Var(v._1))

          changed(Seq(
            Assign(Seq(sizeSym), PathAccess(exp, SizeLink).typed(TScalaInt)),
            Assign(Seq(successSym), Count(Call(funsym, args).typed(ty)).typed(TScalaInt)),
            Assert(Eq(Var(sizeSym), Var(successSym)))
          ))
        case ty => throw new IllegalArgumentException(s"Forall loop expression $exp must have iterable type, but was type $ty")
      }

      case Exists(name, exp, body) => exp.typ.getOrElse(throw new IllegalArgumentException(s"Cannot support exists over untyped expression $exp")) match {
        case TList(ty) =>
          val funsym = Name(gensym.fresh("existsCond"))
          val successSym = Name(gensym.fresh("successSize"))

          val vars = makeCondFun(name, exp, body, ty, funsym)
          val args = vars.map(v => Var(v._1))

          val assign = Assign(Seq(successSym), Count(Call(funsym, args).typed(ty)).typed(TScalaInt))

          val evalParam = EvalParam(successSym).resolved(assign).typed(TScalaInt)
          import scala.meta.quasiquotes._
          val code = Scala[Term](q"${Term.Name(successSym.name)} >= 1")
          val eval = Assert(Eval(Seq(evalParam), code).typed(TScalaBoolean))

          changed(Seq(assign, eval))

        case ty => throw new IllegalArgumentException(s"Forall loop expression $exp must have iterable type, but was type $ty")
      }

      case _ => super.desugarStm(stm)
    }

    private def makeCondFun(name: Name, exp: Expression, body: Body, ty: TLinked, funsym: Name)(implicit gensym: Gensym): Seq[(Name, Option[Type])] = {
      val newbody = Body(Seq(
        Foreach(name, exp,
          Body(body.stmts.flatMap(desugarStm) :+ Yield(Var(name)))
        )
      ))
      val vars = newbody.freeVars.toSeq
      val params = vars.map(v => Param(v._1, v._2.getOrElse(TAny)))
      forallExistsFuns += PatternFunction(None, funsym, params, ty, Seq(newbody))
      vars
    }

    override def desugarFun(fun: PatternFunction)(implicit gensym: Gensym): Seq[PatternFunction] = {
      val desugared = super.desugarFun(fun)
      if (forallExistsFuns.isEmpty)
        desugared
      else {
        val prepend = forallExistsFuns.toSeq
        forallExistsFuns.clear()
        prepend ++ desugared
      }
    }
  }
}