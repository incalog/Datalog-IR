package org.inca.gen.gp.helper

import org.inca.gen.gp.model.PrimitiveConstants.Primitive
import org.inca.lang.core.Content.{IParameter, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint}
import org.inca.lang.gp.Element.GeneratedParameter

import scala.meta._
import org.inca.gen.gp.model.Gensym._
import org.inca.gen.gp.model.Prefix._
import org.inca.gen.gp.helper.Util.asTypeSelect

object GenVariables {

  def createTemporaryVariables(names: List[String]): List[Stat] =
    names.map { name =>
      q"val ${Pat.Var(Term.Name(s"var__$name"))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"
    }

  def localGlobalVariables(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.map { gp =>
      q"val ${Pat.Var(Term.Name(s"var_${gp.name}"))}: PVariable = body.getOrCreateVariableByName(${Lit.String(gp.name)})"
    }.toList

  def temporaryVariables(body: Seq[IPatternBodyContent]): List[String] =
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

  def uniquePrimitives(body: Seq[IPatternBodyContent]): List[Primitive] =
    body.collect {
      case GraphPatternCompareConstraint(_, left, right) =>
        List(left, right).collect { case p: Primitive => p }
    }.toList.flatten.distinct

  def generatedTemporaryVariables(body: Seq[IPatternBodyContent]): List[String] =
    body.collect {
      case p: PathExpressionConstraint =>
        p.trg match {
          case t: TemporaryVariable with GeneratedParameter => t.name
          case _ => ""
        }
    }.toList.distinct.filterNot(x => x.isEmpty)

  def pparams(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.toList map { gp =>
      val name = s"p_${gp.name}"
      val primitiveTypeName = asTypeSelect(gp.typ.get.toString)
      val pConceptKey = q"new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[$primitiveTypeName]))"

      q"""private val ${Pat.Var(Term.Name(name))}: PParameter =
            new PParameter(${Lit.String(name)},
            ${Lit.String(gp.typ.get.toString.tail)},
            $pConceptKey)"""
    }
}
