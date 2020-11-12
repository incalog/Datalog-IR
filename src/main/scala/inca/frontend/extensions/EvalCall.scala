package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.Frontend
import inca.frontend.core.{Expression, _}
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.parser.SourceLocation
import inca.frontend.util.TypeHelper
import inca.util.Gensym
import inca.util.Meta.Scala

import scala.collection.mutable.ListBuffer
import scala.meta.parsers._

case class EvalCall(fun: Scala[meta.Term], args: Seq[Expression]) extends Expression with SourceLocation {
  override def freeVars: Map[Name, Option[Type]] = args.flatMap(_.freeVars).toMap

  override def prettyprint(implicit indent: String): String =
    s"`${fun.syntax}`(${args.map(_.prettyprint).mkString(", ")})"
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
        case Left(typ) if typ.endsWith(".type") =>
          typecheckScala(s"${fun.tree.syntax}.apply _") match {
            case Left(applyTyp) => applyTyp
            case Right(err) =>
              error(err.getMessage, exp)
              "Any"
          }
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
            case Right(paramTy) =>
              if (!subtype(argTy, paramTy, lang)) {
                error(s"Cannot pass argument of type $argTy to $param of type $paramTy", arg)
              }
            case Left(msg) =>
              error(msg, exp)
          }
      }

      TScala(Scala(result))

    case _ => super.typecheckInternal(exp, anno)
  }
}

object EvalCall extends Desugarable {
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

        val code = meta.Term.Apply(fun.tree, params.map(n => meta.Term.Name(n.name.name)).toList)
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