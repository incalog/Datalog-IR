package org.inca.lang.gp

import org.inca.lang.core.Constraints._
import org.inca.lang.core.ITypeConstraintProvider
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Values.{IValue, IVariableValue}
import org.inca.lang.gp.Content.IGraphPatternBodyContent
import org.inca.meta.MetaElements.{MetaElement, NodeType}


// todo style guide formatting
object Constraints {
  // todo eval func should be ` => Boolean`
  case class GraphPatternCheckConstraint(evalFunc: Boolean)
    extends CheckConstraint(evalFunc) with IGraphPatternBodyContent

  case class GraphPatternCompareConstraint(feature: CompareFeature, left: IValue, right: IValue)
    extends CompareConstraint(feature, left, right) with IGraphPatternBodyContent

  case class GraphPatternConceptConstraint(vari: IVariableValue, typ: MetaElement)
    extends ConceptConstraint(vari, typ) with IGraphPatternBodyContent

  case class PathExpressionConstraint(src: VariableReference,
                                      trg: IValue,
                                      element: IPathElement,
                                      typ: NodeType)
    extends ITypeConstraintProvider with IGraphPatternBodyContent with IPathExpressionLike

  case class PatternCompositionConstraint(neg: Boolean, call: IPatternCall)
    extends IGraphPatternBodyContent with ITypeConstraintProvider

  case class ValueWrapperConstraint(value: IValue)
    extends IGraphPatternBodyContent
}
