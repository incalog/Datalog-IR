package inca.frontend.functional.foreign

import inca.frontend.functional.compile.GenerateIR
import inca.ir
import inca.ir.extension.foreign.{ForeignAggregationOperator, ForeignLanguage}
import inca.frontend.functional.syntax.*
import inca.frontend.functional.typechecker.Typechecker

object FunctionalInca extends ForeignLanguage:
  type Code = FunctionDef

  def compileType(ty: Type): ir.Type =
    new GenerateIR().compileType(ty)


case class FunctionalIncaAggregationOperator(code: FunctionDef) extends ForeignAggregationOperator:
  override val lang: FunctionalInca.type = FunctionalInca

  val tfun@TFun(from, to) = code.funType
  if (from.size != 2 || from.head != from(1) || from.head != to)
    throw new IllegalArgumentException(s"Aggregation operator must be of type (A,A)=>A but got $tfun")
  val aggType = from.head

  override def typecheck(in: Seq[ir.Type]): Either[String, ir.Type] =
    if (in.size != 1)
      Left(s"Function $code expects 1 argument, but found ${in.size} arguments in call")
    else if (FunctionalInca.compileType(aggType) != in.head)
      Left(s"Invalid argument of type ${in.head} for parameter of type $aggType")
    else
      Right(FunctionalInca.compileType(aggType))
