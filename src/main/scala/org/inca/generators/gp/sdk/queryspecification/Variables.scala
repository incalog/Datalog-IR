package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.Primitives.Primitive
import org.inca.lang.core.Content.{IParameter, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint}
import org.inca.lang.gp.Element.GeneratedParameter

import scala.meta._
import org.inca.generators.gp.sdk.queryspecification.VariableDissolver._

object Variables {

  def createTemporaryVariables(names: List[String]): List[Stat] = for (name <- names) yield {
    q"val  ${tempVarTermName(name)}: PVariable = body.getOrCreateVariableByName(${stringToLit(name)})"
  }

  def createLocalGlobalVariables(graphParameters: Seq[IParameter]): List[Stat] =
    (for (gp <- graphParameters) yield {
      q"val ${pVarVarName(gp.name)}: PVariable = body.getOrCreateVariableByName(${stringToLit(gp.name)})"
    }).toList


  def getTemporaryVariables(body: Seq[IPatternBodyContent]): List[String] =
    body.collect {
      case p: PathExpressionConstraint =>
        p.trg match {
          case t: TemporaryVariable => t.name
          case _ => ""
        }
    }.toList.distinct.filterNot(x => x.isEmpty)

  def primitivesToParams(primitives: List[Primitive]): List[Stat] = for (primitive <- primitives) yield {
    q"val ${tempVarTermName(getLabel(primitive))} = body.newConstantVariable(${primitiveLit(primitive)})"
  }


  def collectUniquePrimitives(body: Seq[IPatternBodyContent]): List[Primitive] =
    body.collect {
      case cc: GraphPatternCompareConstraint =>
        List(cc.left, cc.right).collect{case p: Primitive => p}
    }.toList.flatten.distinct

  def getGeneratedTemporaryVariables(body: Seq[IPatternBodyContent]): List[String] =
    body.collect {
      case p: PathExpressionConstraint =>
        p.trg match {
          case t: TemporaryVariable with GeneratedParameter => t.name
          case _ => ""
        }

    }.toList.distinct.filterNot(x => x.isEmpty)
}
