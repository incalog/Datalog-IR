package inca.frontend.constraint.extensions.evalCall

import inca.frontend.constraint.core._
import inca.frontend.constraint.desugar.{DesugarTrans, Desugarable}
import inca.frontend.constraint.extensions.evalCall.Trees._
import inca.util.{Gensym, Scala}

import scala.collection.mutable.ListBuffer

object Desugaring extends Desugarable {

  override def trans(): DesugarTrans = new DesugarTrans {
    private val evalCallAssigns: ListBuffer[Assign] = ListBuffer()

    override def desugarExp(exp: Expression)(implicit gensym: Gensym): Expression = exp match {
      case call@EvalCall(fun, args) =>
        val params = args.map { arg =>
          val sym = Name(gensym.fresh("evalCallArg"))
          val assign = Assign(Seq(sym), arg)
          evalCallAssigns += assign
          EvalParam(sym).resolved(assign).mtyped(arg.typ)
        }

        val code = meta.Term.Apply(fun.code.tree, params.map(n => meta.Term.Name(n.name.name)).toList)
        changed(Eval(params,  Scala(code)).mtyped(call.typ))
      case _ => super.desugarExp(exp)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      if (evalCallAssigns.isEmpty)
        desugared
      else {
        val prepend = evalCallAssigns.toSeq
        evalCallAssigns.clear()
        prepend ++ desugared
      }
    }
  }
}