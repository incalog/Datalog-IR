package org.inca.lang.gp

import org.inca.lang.core.Constraints.{IGeneratorPathElement, IPathElement}
import org.inca.lang.gp.Element.AbstractPathElement
import org.inca.lang.meta.Link

object Gen {
  case class GenListPathElement(isFirst: Boolean, next: Option[IPathElement], link: Link)
    extends AbstractPathElement(next, link) with IGeneratorPathElement

  case class GenIsDefinedPathElement(next: Option[IPathElement], link: Link)
    extends AbstractPathElement(next, link) with IGeneratorPathElement
}
