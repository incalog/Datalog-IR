package org.inca.lang

import org.inca.lang.Core._
import org.inca.meta.MetaElements.{Link, MetaElement, NodeType}

object Gp {

  // content
  trait GraphPatternBodyContent extends PatternBodyContent

  abstract class PatternParameter(name: String, typ: Option[MetaElement])

  case class GraphPattern(name: String,
                          parameters: Seq[Parameter],
                          bodies: Seq[PatternBody],
                          visibility: Option[PatternVisibility]) extends Pattern with PatternModuleContent

  case class GraphPatternBody(contents: Seq[PatternBodyContent]) extends PatternBody

  case class GraphPatternParameter(name: String, typ: Option[MetaElement])
    extends PatternParameter(name, typ) with Parameter

  // path
  trait PathElement {
    val next: Option[PathElement]
    val link: Link
  }

  trait VirtualPathElement extends PathElement

  case class PathElementImpl(next: Option[PathElement], link: Link) extends PathElement

  case class NextPathElement(next: Option[PathElement], link: Link) extends VirtualPathElement

  case class ParentPathElement(next: Option[PathElement], link: Link) extends VirtualPathElement

  case class PrevPathElement(next: Option[PathElement], link: Link) extends VirtualPathElement

  // constraints
  case class CompareConstraint(feature: CompareFeature, left: Value, right: Value) extends GraphPatternBodyContent

  case class ConceptConstraint(vari: VariableValue, typ: NodeType) extends GraphPatternBodyContent

  case class CompositionConstraint(neg: Boolean, call: PatternCall) extends GraphPatternBodyContent

  case class PathExpressionConstraint(src: VariableReference,
                                      trg: Value,
                                      element: PathElement,
                                      typ: NodeType) extends GraphPatternBodyContent


  trait CompareFeature

  case class EqualityCompareFeature() extends CompareFeature

  case class InequalityCompareFeature() extends CompareFeature

}
