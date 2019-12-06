package org.inca.gp

import org.inca.core.ITypeConstraintProvider
import org.inca.core.typ.ICompileTimeIncAType
import org.inca.core.constraints._
import org.inca.core.reference.VariableReference
import org.inca.core.values.{IValue, IVariableValue}
import org.inca.gp_old.content.IGraphPatternBodyContent
import org.inca.mps.binaryOperations.Expression


// todo style guide formatting
object Constraints {
  case class GraphPatternCheckConstraint(expression: Expression) extends CheckConstraint(expression) with IGraphPatternBodyContent
  case class GraphPatternCompareConstraint(feature: CompareFeature, left: IValue, right: IValue)
    extends CompareConstraint(feature, left, right) with IGraphPatternBodyContent
  case class GraphPatternConceptConstraint(vari: IVariableValue, typ: ICompileTimeIncAType)
    extends ConceptConstraint(vari, typ) with IGraphPatternBodyContent
  case class PathExpressionConstraint(src: VariableReference, trg: IValue, element: IPathElement, typ: ICompileTimeIncAType)
    extends ITypeConstraintProvider with IGraphPatternBodyContent with IPathExpressionLike
  case class PatternCompositionConstraint(neg: Boolean, call: IPatternCall) extends IGraphPatternBodyContent with ITypeConstraintProvider
  case class ValueWrapperConstraint(value: IValue) extends IGraphPatternBodyContent
}
