package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core.CompileToGP.resolveDataOp
import inca.frontend.core.Core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.util.{Gensym, Meta}

import scala.collection.mutable.ListBuffer
import scala.meta.Term

case class DataOpCall(op: DataOp, args: Seq[Exp]) extends Exp {
  override def freeVars: Map[Name, Option[TypeAnno]] = args.flatMap(_.freeVars).toMap

  override def prettyprint(implicit indent: String): String =
    s"${op.prettyprint}(${args.map(_.prettyprint).mkString(", ")})"
}

/** Extension adding dataop operations to @see Parser.
 *
 * @todo Ambiguous Syntax => Same as Call
 */
trait DataOpCallFrontentd extends Frontend {
  override protected def desugarables: Seq[Desugarable] = DataOpCall +: super.desugarables
}

object DataOpCall extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    private val dataOpAssigns: ListBuffer[Assign] = ListBuffer()

    override def desugarExp(exp: Exp)(implicit gensym: Gensym): Exp = exp match {
      case call@DataOpCall(op, args) =>
        val syms = args.map { arg =>
          val sym = Name(gensym.fresh("dataOpArg"))
          dataOpAssigns += Assign(Seq(sym), arg)
          sym
        }

        val resultType = call.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped data op call $call"))
        val qop = resolveDataOp(op)
        val fun = Meta.mkQualName(qop)
        val code: Term = if(syms.isEmpty) fun else Term.Apply(fun, syms.map(n => Meta.mkQualName(n.name)).toList)
        changed(Eval(syms, code).typed(resultType))
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