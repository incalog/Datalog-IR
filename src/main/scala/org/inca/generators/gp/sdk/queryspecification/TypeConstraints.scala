package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.Primitives.Primitive
import org.inca.generators.gp.sdk.queryspecification.VariableDissolver._
import org.inca.lang.core.Constraints.{EqualityCompareFeature, InequalityCompareFeature}
import org.inca.lang.core.Content.{IParameter, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Values.IValue
import org.inca.lang.gp.Constraints._
import org.inca.lang.gp.Content.GraphPatternParameter

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
    val left = compareType(cc.left)
    val right = compareType(cc.right)
    cc.feature match {
      case _: EqualityCompareFeature   => q"new Equality(body, $left, $right)"
      case _: InequalityCompareFeature => q"new Inequality(body, $left, $right)"
    }
  }

  // todo rename
  private def compareType(value: IValue): Term.Name = {
    value match {
      case VariableReference(v) => asBodyVar(v.name).toTerm
      // todo change when primitives are taken care of
      case p: Primitive => asVar(getLabel(p)).toTerm
    }
  }

}
