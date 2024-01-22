package inca.frontend.oodl.foreign

import inca.frontend.oodl.compile.GenerateIR
import inca.ir
import inca.frontend.oodl.syntax.{Expression, FunctionDef, Type}
import inca.ir.Name
import inca.ir.extension.foreign.{ForeignAggregationOperator, ForeignLanguage}

object OODL extends ForeignLanguage:
  type Code = Expression

  def compileType(ty: Type): ir.Type =
    new GenerateIR().compileType(ty)

case class OODLAggregationOperator(code: FunctionDef,  initCode: Expression, addCode: Expression)extends ForeignAggregationOperator:
  override val lang: OODL.type = OODL
  override val name: Name = code.name

  val (from, to) = (code.params.map(_.typ), code.outType)
  if (from.size != 2 || from.head != from(1) || from.head != to)
    throw new IllegalArgumentException(s"Aggregation operator must be of type (A,A)=>A but got ${from.mkString("(", ",", ")")} => $to")
  val aggType = from.head

  override def resultType: ir.Type = OODL.compileType(aggType)

  override def typecheck(in: Seq[ir.Type]): Option[String] =
      if (in.size != 1)
        Some(s"Function $code expects 1 argument, but found ${in.size} arguments in call")
      else if (OODL.compileType(aggType) != in.head)
        Some(s"Invalid argument of type ${in.head} for parameter of type $aggType")
      else
        None
