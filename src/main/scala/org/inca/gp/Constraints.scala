package org.inca.gp

import org.inca.core.Constraints._
import org.inca.core.ITypeConstraintProvider
import org.inca.core.Reference.VariableReference
import org.inca.core.Typp.Typ
import org.inca.core.Values.{IValue, IVariableValue}
import org.inca.gp.Content.IGraphPatternBodyContent
import org.inca.mps.binaryOperations.Expression


// todo style guide formatting
object Constraints {
  case class GraphPatternCheckConstraint(expression: Expression)
    extends CheckConstraint(expression) with IGraphPatternBodyContent

  case class GraphPatternCompareConstraint(feature: CompareFeature, left: IValue, right: IValue)
    extends CompareConstraint(feature, left, right) with IGraphPatternBodyContent

  case class GraphPatternConceptConstraint(vari: IVariableValue, typ: Typ)
    extends ConceptConstraint(vari, typ) with IGraphPatternBodyContent

  case class PathExpressionConstraint(src: VariableReference,
                                      trg: IValue,
                                      element: IPathElement,
                                      typ: Typ)
    extends ITypeConstraintProvider with IGraphPatternBodyContent with IPathExpressionLike

  // todo should `neg` be default false? - see usage in MPS `TestPatterns`
  case class PatternCompositionConstraint(neg: Boolean, call: IPatternCall)
    extends IGraphPatternBodyContent with ITypeConstraintProvider

  case class ValueWrapperConstraint(value: IValue)
    extends IGraphPatternBodyContent
}
