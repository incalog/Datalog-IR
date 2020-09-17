package inca.frontend.extensions

import inca.frontend.core.CompileToGP.resolveDataOp
import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer
import scala.meta.Term

case class DataOpCall(op: DataOp, args: Seq[Exp]) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = args.flatMap(_.freeVars).toMap

  override def prettyprint(implicit indent: String): String =
    s"${op.prettyprint}(${args.map(_.prettyprint).mkString(", ")})"
}


object DataOpCall extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    private val dataOpAssigns: ListBuffer[Assign] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case call@DataOpCall(op, args) =>
        val syms = args.map { arg =>
          val sym = gensym.fresh("dataOpArg")
          dataOpAssigns += Assign(Seq(sym), arg)
          sym
        }

        val resultType = call.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped data op call $call"))
        val argString = if (syms.isEmpty) "" else s"(${syms.mkString(", ")})"
        val qop = resolveDataOp(op)
        val code: Term = if(syms.isEmpty) Term.Name(qop) else Term.Apply(Term.Name(qop), syms.map(Term.Name(_)).toList)
        s"$qop$argString"
        changed(Eval(syms, code))
      case _ => super.desugarExp(exp)
    }

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
      val desugared = super.desugarStm(stm)
      if (dataOpAssigns.isEmpty)
        desugared
      else {
        val prepend = dataOpAssigns.toSeq
        dataOpAssigns.clear()
        prepend ++ desugared
      }
    }
  }
}