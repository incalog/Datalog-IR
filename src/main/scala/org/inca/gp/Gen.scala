package org.inca.gp

import org.inca.core.Constraints.{IGeneratorPathElement, IPathElement}
import org.inca.gp.Element.AbstractPathElement
import org.inca.mps.Link

object Gen {
  case class GenListPathElement(isFirst: Boolean, next: Option[IPathElement], link: Link)
    extends AbstractPathElement(next, link) with IGeneratorPathElement

  case class GenIsDefinedPathElement(next: Option[IPathElement], link: Link)
    extends AbstractPathElement(next, link) with IGeneratorPathElement
}
