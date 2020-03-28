package org.inca.lang.gp

import org.inca.lang.core.Constraints.IPathElement
import org.inca.lang.core.Content._
import org.inca.meta.MetaElements.{Link, MetaElement}


object Content {

  trait GraphPatternBodyContent extends PatternBodyContent

  trait GeneratedParameter

  case class PathElement(next: Option[IPathElement], link: Link) extends IPathElement

  abstract class PatternParameter(name: String, typ: Option[MetaElement])

  case class EmptyGraphPatternContent()
    extends EmptyContent with GraphPatternBodyContent with PatternModuleContent

  case class GraphPattern(name: String,
                          parameters: Seq[Parameter],
                          bodies: Seq[PatternBody],
                          visibility: Option[PatternVisibility]) extends Pattern with PatternModuleContent

  case class GraphPatternBody(contents: Seq[PatternBodyContent]) extends PatternBody

  case class GraphPatternParameter(name: String, typ: Option[MetaElement])
    extends PatternParameter(name, typ) with Parameter

}
