package org.inca.core

import org.inca.core.Content.{IPattern, IPatternBodyContent}
import org.inca.core.Misc.{IContainsJavaExpression, IJavaContext, ITransformable}
import org.inca.core.Typ.ICompileTimeIncAType
import org.inca.core.Values.{IValue, IVariableValue}
import org.inca.mps.InterfacePart
import org.inca.mps.binaryOperations.Expression

object Constraints {
  trait IGeneratorPathElement extends IVirtualPathElement
  trait IPathElementScopeProvider
  trait IPathExpressionLike extends IPathElementScopeProvider

  trait IPathElement extends IPathElementScopeProvider with ITransformable {
    val next: Option[IPathElement]
    val interfacePart: InterfacePart
  }
  trait IPatternCall {
    val transitive: Boolean
    val arguments: Seq[IValue]
    val pattern: IPattern
  }

  // todo `expression` mps removal
  abstract class CheckConstraint(expression: Expression)
    extends IPatternBodyContent
      with IJavaContext
      with IContainsJavaExpression
  abstract class CompareConstraint(feature: CompareFeature,left: IValue, right: IValue)
    extends IPatternBodyContent
      with ITypeConstraintProvider
  abstract class ConceptConstraint(vari: IVariableValue, typ: ICompileTimeIncAType)
    extends IPatternBodyContent with ITypeConstraintProvider

  case class PatternCall(transitive: Boolean, arguments: Seq[IValue], pattern: IPattern) extends IPatternCall

  // enum
  // todo how to implement enums?
  trait CompareFeature
  case class EqualityCompareFeature() extends CompareFeature
  case class InequalityCompareFeature() extends CompareFeature
}
