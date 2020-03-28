package org.inca.lang.gp

import org.inca.lang.core.Constraints._
import org.inca.lang.core.ITypeConstraintProvider
import org.inca.lang.core.Reference.CoreVariableReference
import org.inca.lang.core.Values.{Value, VariableValue}
import org.inca.lang.gp.Content.GraphPatternBodyContent
import org.inca.meta.MetaElements.NodeType


object Constraints {
  // todo IValue -> AnyVal?
  case class GraphPatternCompareConstraint(feature: CompareFeature, left: Value, right: Value)
    extends CompareConstraint(feature, left, right) with GraphPatternBodyContent

  case class GraphPatternConceptConstraint(vari: VariableValue, typ: NodeType)
    extends ConceptConstraint(vari, typ) with GraphPatternBodyContent

  case class PathExpressionConstraint(src: CoreVariableReference,
                                      trg: Value,
                                      element: IPathElement,
                                      typ: NodeType)
    extends ITypeConstraintProvider with GraphPatternBodyContent with IPathExpressionLike

  case class PatternCompositionConstraint(neg: Boolean, call: IPatternCall)
    extends GraphPatternBodyContent with ITypeConstraintProvider

}
