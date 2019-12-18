package org.inca.lang.core

import org.inca.lang.core.Content.{IPattern, IPatternBodyContent}
import org.inca.lang.core.Misc.{IContainsJavaExpression, IJavaContext, ITransformable}
import org.inca.lang.core.Typp.Typ
import org.inca.lang.core.Values.{IValue, IVariableValue}
import org.inca.lang.mps.Link

object Constraints {
  trait IGeneratorPathElement extends IVirtualPathElement
  trait IPathElementScopeProvider
  trait IPathExpressionLike extends IPathElementScopeProvider

  trait IPathElement extends IPathElementScopeProvider with ITransformable {
    val next: Option[IPathElement]
    val link: Link
  }
  trait IPatternCall {
    val transitive: Boolean
    val arguments: Seq[IValue]
    val pattern: IPattern
  }

  abstract class CheckConstraint(evalFunc: => Boolean)
    extends IPatternBodyContent with IJavaContext with IContainsJavaExpression

  abstract class CompareConstraint(feature: CompareFeature,left: IValue, right: IValue)
    extends IPatternBodyContent with ITypeConstraintProvider

  abstract class ConceptConstraint(vari: IVariableValue, typ: Typ)
    extends IPatternBodyContent with ITypeConstraintProvider

  case class PatternCall(transitive: Boolean, arguments: Seq[IValue], pattern: IPattern) extends IPatternCall

  // enum
  trait CompareFeature
  case class EqualityCompareFeature() extends CompareFeature
  case class InequalityCompareFeature() extends CompareFeature

  case class Something(str: String)
    extends IValue with Typ
}
