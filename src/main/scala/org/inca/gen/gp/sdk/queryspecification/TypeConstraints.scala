package org.inca.gen.gp.sdk.queryspecification

import VariableDissolver._
import org.inca.lang.core.Constraints.{EqualityCompareFeature, InequalityCompareFeature}
import org.inca.lang.core.Content.{IParameter, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Values.IValue
import org.inca.lang.gp.Constraints._
import org.inca.lang.gp.Content.GraphPatternParameter

import Gensym._
import Prefix._

import scala.meta._

object TypeConstraints {

  def createTypeConstraintsParameters(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.map { param =>
      q"""new TypeConstraint(
         body,
         Tuples.flatTupleOf(${asBodyVar(param.name).toTerm}),
         new ClassKey(NodeType(classOf[${param.typ.get.toString.toClassPath}]))
       )"""
    }.toList

  def createTypeConstraints(bodyContent: Seq[IPatternBodyContent]): List[Stat] =
    bodyContent.collect {
      case pxc: PathExpressionConstraint => createPathExpressionConstraint(pxc)
      case pcc: PatternCompositionConstraint => createPatternCompositionConstraint(pcc)
      case cc: GraphPatternCompareConstraint => createGraphPatternCompareConstraint(cc)
    }.toList

  private def createPathExpressionConstraint(pxc: PathExpressionConstraint): Stat = {
    val src = pxc.src.variable match {
      case TemporaryVariable(name, _) => Term.Name(s"var__$name")
      case GraphPatternParameter(name, _) => Term.Name(s"var_$name")
    }
    val trg = pxc.trg match {
      case VariableReference(v) => Term.Name(s"var_${v.name}")
      case TemporaryVariable(name, _) => Term.Name(s"var__$name")
    }
    q"""new TypeConstraint(
             body,
             Tuples.staticArityFlatTupleOf($src, $trg),
             new LinkKey(NodeType(
                classOf[${pxc.typ.toString.toClassPath}])
                  (${pxc.element.link.fld.getName.toLit}))
           )"""
  }

  private def createPatternCompositionConstraint(pcc: PatternCompositionConstraint): Stat = {
    val args = getPatternCompConstrArguments(pcc.call.arguments)
    val patternName = Type.Name(s"${pcc.call.pattern.name}_QuerySpecification")
    q"""
       new PositivePatternCall(
          body,
          Tuples.flatTupleOf(..$args),
          new $patternName().instance().getInternalQueryRepresentation()
       )
     """
  }

  private def getPatternCompConstrArguments(args: Seq[IValue]): List[Term] =
    args.map {
      case VariableReference(v) => v match {
        case GraphPatternParameter(name, _) => asBodyVar(name).toTerm
        case TemporaryVariable(name, _) => asVar(name).toTerm
      }
      case TemporaryVariable(name, _) => asBodyVar(name).toTerm
    }.toList

  private def createGraphPatternCompareConstraint(cc: GraphPatternCompareConstraint): Stat = {
    val left = getTermNameLabel(cc.left)
    val right = getTermNameLabel(cc.right)
    cc.feature match {
      case _: EqualityCompareFeature   => q"new Equality(body, $left, $right)"
      case _: InequalityCompareFeature => q"new Inequality(body, $left, $right)"
    }
  }

  private def getTermNameLabel(value: Any): Term.Name = {
    value match {
      case VariableReference(v) => asBodyVar(v.name).toTerm
      case _ => Term.Name(generateLabel(var__, value))
    }
  }
}