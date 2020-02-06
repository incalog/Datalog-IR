package org.inca.generators.gp.sdk.queryspecification

import org.inca.generators.gp.sdk.queryspecification.Primitives.Primitive
import org.inca.generators.gp.util.Util.classPathToTypeSelect
import org.inca.lang.core.Constraints.{EqualityCompareFeature, InequalityCompareFeature}
import org.inca.lang.core.Content.{IParameter, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Values.IValue
import org.inca.lang.gp.Constraints.{GraphPatternCompareConstraint, PathExpressionConstraint, PatternCompositionConstraint}
import org.inca.lang.gp.Content.GraphPatternParameter
import VariableDissolver._

import scala.meta._

object TypeConstraints {

  def createTypeConstraintsParameters(graphParameters: Seq[IParameter]): List[Stat] =
    (for (graphParameter <- graphParameters) yield {
      q"""new TypeConstraint(
         body,
         Tuples.flatTupleOf(${asBodyVar(graphParameter.name).toTerm}),
         new ClassKey(NodeType(classOf[${graphParameter.typ.get.toString.toClassPath}]))
       )"""
    }).toList

  def createTypeConstraints(bodyContent: Seq[IPatternBodyContent]): List[Stat] =
    bodyContent.collect {
      case pxc: PathExpressionConstraint => createPathExpressionConstraint(pxc)
      case pcc: PatternCompositionConstraint => createPatternCompositionConstraint(pcc)
      case cc: GraphPatternCompareConstraint => createGraphPatternCompareConstraint(cc)
    }.toList

  private def createPathExpressionConstraint(pxc: PathExpressionConstraint): Stat = {
    val src = pxc.src.variable match {
      case t: TemporaryVariable =>
        Term.Name(s"var__${t.name}")
      case gpp: GraphPatternParameter =>
        Term.Name(s"var_${gpp.name}")
    }
    val trg = pxc.trg match {
      case vr: VariableReference =>
        Term.Name(s"var_${vr.variable.name}")

      case tv: TemporaryVariable =>
        Term.Name(s"var__${tv.name}")
    }
    q"""new TypeConstraint(
             body,
             Tuples.staticArityFlatTupleOf($src, $trg),
             new LinkKey(NodeType(classOf[${pxc.typ.toString.toClassPath}])(${pxc.element.link.fld.getName.toLit}))
           )"""
  }

  private def createPatternCompositionConstraint(pcc: PatternCompositionConstraint): Stat = {
    val args: List[Term.Name] = (for (arg <- pcc.call.arguments) yield {
      arg match {
        case vr: VariableReference =>
          vr.variable match {
            case gpp: GraphPatternParameter =>
              asBodyVar(gpp.name).toTerm
            case tv: TemporaryVariable =>
              asVar(tv.name).toTerm
          }
        case tv: TemporaryVariable =>
          asBodyVar(tv.name).toTerm
      }
    }).toList
    // todo change mocked file name
    val mockClassName = "GPLang"
    q"""
       new PositivePatternCall(
          body,
          Tuples.flatTupleOf(..$args),
          new ${s"${pcc.call.pattern.name}_${mockClassName}QuerySpecification".toType}().instance().getInternalQueryRepresentation()
       )
     """
  }

  private def createGraphPatternCompareConstraint(cc: GraphPatternCompareConstraint): Stat = {
    val left = compareType(cc.left)
    val right = compareType(cc.right)
    cc.feature match {
      case _: EqualityCompareFeature =>
        q"""
          new Equality(body, $left, $right)
        """
      case _: InequalityCompareFeature =>
        q"""
          new Inequality(body, $left, $right)
        """
    }
  }

  private def compareType(value: IValue): Term.Name = {
    value match {
      case vr: VariableReference => asBodyVar(vr.variable.name).toTerm
      case p: Primitive => asVar(getLabel(p)).toTerm
    }
  }

}
