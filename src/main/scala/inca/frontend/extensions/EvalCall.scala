package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.Frontend
import inca.frontend.core.{Expression, _}
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.TypeOps
import inca.frontend.util.TypeHelper
import inca.util.Meta.Scala

import scala.collection.mutable.ListBuffer
import scala.meta.parsers._

case class EvalCall(fun: Scala[meta.Term], args: Seq[Expression]) extends Expression with SourceLocation {
  override def freeVars: Map[Name, Option[Type]] = args.flatMap(_.freeVars).toMap

  override def prettyprint(implicit indent: String): String =
    s"${fun.syntax}(${args.map(_.prettyprint).mkString(", ")})"
}

/** Extension adding dataop operations to @see Parser.
 */
trait EvalCallFrontend extends Frontend {
  override protected def desugarables: Seq[Desugarable] = EvalCall +: super.desugarables


  override protected[frontend] def atomicExp[_: P]: P[Expression] =
    P("`" ~ evalCore ~ "`" ~ "(" ~ exp.rep(sep = ",") ~ ")").flatMapWithLoc { case (eval, args) =>
      if (eval.params.nonEmpty)
        fastparse.Fail
      else
        fastparse.Pass(EvalCall(eval.code, args))
    } | super.atomicExp


  override protected def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case EvalCall(fun, args) =>
      val typString = typecheckScala(fun.tree.syntax) match {
        case Left(typ) =>
          typ
        case Right(err) =>
          error(err.getMessage, exp)
          "Any"
      }

      val (params, result) = typString.parse[meta.Type].get match {
        case meta.Type.Function(params, result) =>
          (params, result)
        case ty =>
          (Seq(), ty)
      }

      if (params.size != args.size) {
        error(s"Function $fun expects ${params.size} arguments, but found ${args.size} arguments in call", exp)
      }

      params.zipAll(args, null, null) foreach {
        case (null, arg) =>
          typecheck(arg)
        case (param, null) =>
        // nothing
        case (param, arg) =>
          val argTy = typecheck(arg)
          TypeHelper.decode(param) match {
            case Left(paramTy) =>
              if (TypeOps.meet(paramTy, argTy, lang) == TNothing) {
                warn(s"Cast of argument type $argTy to unrelated parameter type ${paramTy} will always fail", arg)
              }
            case Right(msg) =>
              error(msg, exp)
          }
      }

      TypeHelper.decode(result) match {
        case Left(ty) => ty
        case Right(msg) =>
          error(msg, exp)
          TAny
      }

    case _ => super.typecheckInternal(exp, anno)
  }
}

object EvalCall extends Desugarable {
  override def trans(): DesugarTrans = new DesugarTrans {
    private val dataOpAssigns: ListBuffer[Assign] = ListBuffer()

//    override def desugarExp(exp: Expression)(implicit gensym: Gensym): Expression = exp match {
//      case call@Call(op, args, false) =>
//        val syms = args.map { arg =>
//          val sym = Name(gensym.fresh("dataOpArg"))
//          dataOpAssigns += Assign(Seq(sym), arg)
//          sym
//        }
//
//        val resultType = call.typ.getOrElse(throw new IllegalArgumentException(s"Cannot compile untyped data op call $call"))
//        val qop = resolveDataOp(op)
//        val fun = Meta.mkQualName(qop)
//        val code: Term = if(syms.isEmpty) fun else Term.Apply(fun, syms.map(n => Meta.mkQualName(n.name)).toList)
//        changed(Eval(syms,  Scala(code)).typed(resultType))
//      case _ => super.desugarExp(exp)
//    }
//
//    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = {
//      val desugared = super.desugarStm(stm)
//      if (dataOpAssigns.isEmpty)
//        desugared
//      else {
//        val prepend = dataOpAssigns.toSeq
//        dataOpAssigns.clear()
//        prepend ++ desugared
//      }
//    }
  }
}