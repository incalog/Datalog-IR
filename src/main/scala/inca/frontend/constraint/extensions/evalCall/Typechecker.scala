package inca.frontend.constraint.extensions.evalCall

import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.evalCall.Trees._
import inca.frontend.constraint.typechecker.{CoreTypechecker, TypeHelper}
import inca.util.Scala

trait Typechecker extends CoreTypechecker {
  override protected def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case EvalCall(fun, args) =>
      val typString = typecheckScala(fun.code.syntax) match {
        case Left(typ) if typ.endsWith(".type") =>
          typecheckScala(s"${fun.code.syntax}.apply _") match {
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

      import scala.meta.parsers._
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
              if (!subtype(argTy, paramTy, dataModel)) {
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
