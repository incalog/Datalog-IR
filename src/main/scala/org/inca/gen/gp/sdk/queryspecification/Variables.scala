package org.inca.gen.gp.sdk.queryspecification

import PrimitiveConstants.Primitive
import org.inca.lang.core.Content.{IParameter, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint}
import org.inca.lang.gp.Element.GeneratedParameter

import scala.meta._
import VariableDissolver._

import Gensym._
import Prefix._

object Variables {

  def createTemporaryVariables(names: List[String]): List[Stat] =
    names.map { name =>
      q"val ${asVar(name).toVar}: PVariable = body.getOrCreateVariableByName(${name.toLit})"
    }

  def createLocalGlobalVariables(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.map { gp =>
      q"val ${asBodyVar(gp.name).toVar}: PVariable = body.getOrCreateVariableByName(${gp.name.toLit})"
    }.toList

  def getTemporaryVariables(body: Seq[IPatternBodyContent]): List[String] =
    body.collect {
      case PathExpressionConstraint(_, trg, _, _) =>
        trg match {
          case TemporaryVariable(name, _) => name
          case _ => ""
        }
    }.toList.distinct.filterNot(x => x.isEmpty)

  def primitivesToParams(primitives: List[Primitive]): List[Stat] =
    primitives.map { primitive =>
      val variable = Pat.Var(Term.Name(generateLabel(var__, primitive)))
      q"val $variable = body.newConstantVariable(${primitiveLit(primitive)})"
    }

  def collectUniquePrimitives(body: Seq[IPatternBodyContent]): List[Primitive] =
    body.collect {
      case GraphPatternCompareConstraint(_, left, right) =>
        List(left, right).collect { case p: Primitive => p }
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
