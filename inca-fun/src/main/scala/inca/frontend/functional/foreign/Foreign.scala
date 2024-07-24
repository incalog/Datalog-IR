package inca.frontend.functional.foreign

import inca.frontend.functional.compile.GenerateIR
import inca.ir
import inca.ir.extension.foreign.{ForeignAggregationOperator, ForeignLanguage, ForeignMonoDefinition}
import inca.frontend.functional.syntax.*
import inca.frontend.functional.typechecker.Typechecker
import inca.ir.Name
import inca.ir.extension.mono.MonoTypes
import inca.util.Gensym

object FunctionalInca extends ForeignLanguage:
  type Code = Expression

  def compileType(ty: Type): ir.Type =
    new GenerateIR().compileType(ty)

// Idea:
// The scala frontend must lower to the inca-foreign-scala IR, otherwise we get a dependency conflict again
case class FunctionalIncaAggregationOperator(code: FunctionDef, initCode: Expression, addCode: Expression) extends ForeignAggregationOperator:
  override val name: Name = code.name
  override val lang: FunctionalInca.type = FunctionalInca

  val tfun@TFun(from, to) = code.funType
  if (from.size != 2 || from.head != from(1) || from.head != to)
    throw new IllegalArgumentException(s"Aggregation operator must be of type (A,A)=>A but got $tfun")
  val aggType = from.head

  override def resultType: ir.Type = FunctionalInca.compileType(aggType)

  override def typecheck(in: Seq[ir.Type]): Option[String] =
    if (in.size != 1)
      Some(s"Function $code expects 1 argument, but found ${in.size} arguments in call")
    else if (FunctionalInca.compileType(aggType) != in.head)
      Some(s"Invalid argument of type ${in.head} for parameter of type $aggType. Did you set the postProcessingPipeline correctly?")
    else
      None

case class FunctionalIncaMonoDefinition(name: Name, initCode: Expression, addCode: Expression, resultCode: Expression, typ: MonoTypes) extends ForeignMonoDefinition:
  override val lang: FunctionalInca.type = FunctionalInca
  val constructorParamTypes: Seq[ir.Type] = Seq()
