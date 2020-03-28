package org.inca.gen.gp.helper

import org.inca.gen.gp.model.Primitive
import org.inca.lang.Core._
import org.inca.lang.Core.Value
import org.inca.lang.Gp._

import scala.meta._
import org.inca.gen.Gensym._
import org.inca.gen.gp.model.Prefix._
import org.inca.gen.gp.helper.Util.asTypeSelect

object GenVariables {

  def createTemporaryVariables(names: List[String]): List[Stat] =
    names.map { name =>
      q"val ${Pat.Var(Term.Name(s"var__$name"))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"
    }

  def localGlobalVariables(graphParameters: Seq[Parameter]): List[Stat] =
    graphParameters.map { gp =>
      q"val ${Pat.Var(Term.Name(s"var_${gp.name}"))}: PVariable = body.getOrCreateVariableByName(${Lit.String(gp.name)})"
    }.toList

  def temporaryVariables(body: Seq[PatternBodyContent]): List[String] =
    body.collect {
      case PathExpressionConstraint(src, trg, _, _) =>
        List[String](
          trg match {
            case CoreTemporaryVariable(name, _) => name
            case _ => ""
          },
          hasRefVar(src)
        )
      case CompareConstraint(_, left, right) =>
        List[String](
          hasRefVar(left),
          hasRefVar(right))
    }.toList.flatten.distinct.filterNot(x => x.isEmpty)

  def primitivesToParams(primitives: List[Primitive]): List[Stat] =
    primitives.map { primitive =>
      val variable = Pat.Var(Term.Name(generateLabel(var__, primitive)))
      q"val $variable = body.newConstantVariable(${primitiveLit(primitive)})"
    }

  def uniquePrimitives(body: Seq[PatternBodyContent]): List[Primitive] =
    body.collect {
      case CompareConstraint(_, left, right) =>
        List(left, right).collect { case p: Primitive => p }
    }.toList.flatten.distinct

  def generatedTemporaryVariables(body: Seq[PatternBodyContent]): List[String] =
    body.collect {
      case p: PathExpressionConstraint =>
        p.trg match {
          case t: CoreTemporaryVariable with GeneratedParameter => t.name
          case _ => ""
        }
    }.toList.distinct.filterNot(x => x.isEmpty)

  def pparams(graphParameters: Seq[Parameter]): List[Stat] =
    graphParameters.toList map { gp =>
      val name = s"p_${gp.name}"
      val primitiveTypeName = asTypeSelect(gp.typ.get.toString)
      val pConceptKey = q"new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[$primitiveTypeName]))"

      q"""private val ${Pat.Var(Term.Name(name))}: PParameter =
            new PParameter(${Lit.String(gp.name)},
            MetaElements.NodeType(classOf[$primitiveTypeName]).toString,
            $pConceptKey)"""
    }

  private def hasRefVar(v: Value): String = v match {
    case CoreVariableReference(v) => v match {
      case CoreTemporaryVariable(name, _) => name
      case _ => ""
    }
    case _ => ""
  }
}
