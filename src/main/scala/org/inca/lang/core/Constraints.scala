package org.inca.lang.core

import org.inca.lang.core.Content.{Pattern, PatternBodyContent}
import org.inca.lang.core.Misc.{IContainsJavaExpression, IJavaContext, ITransformable}
import org.inca.lang.core.Values.{Value, VariableValue}
import org.inca.meta.MetaElements.{Link, MetaElement, NodeType}

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
    val arguments: Seq[Value]
    val pattern: Pattern
  }
  // todo check implementation
  trait ContextPointer {
    val index: Integer
    val parent: Any
    val next: Option[Any]
    val prev: Option[Any]
    val first: Option[Any]
    val last: Option[Any]
  }

  abstract class CompareConstraint(feature: CompareFeature, left: Value, right: Value)
    extends PatternBodyContent with ITypeConstraintProvider

  abstract class ConceptConstraint(vari: VariableValue, typ: NodeType)
    extends PatternBodyContent with ITypeConstraintProvider

  case class PatternCall(transitive: Boolean, arguments: Seq[Value], pattern: Pattern) extends IPatternCall

  // enum
  trait CompareFeature
  case class EqualityCompareFeature() extends CompareFeature
  case class InequalityCompareFeature() extends CompareFeature
}
