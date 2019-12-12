package org.inca.gp

import org.inca.core.Constraints.IPathElement
import org.inca.core.IVirtualPathElement
import org.inca.mps.Link

object Element {
  abstract class AbstractListPathElement(next: Option[IPathElement], link: Link)
    extends IPathElement with IVirtualPathElement

  abstract class AbstractPathElement(next: Option[IPathElement], link: Link) extends IPathElement


  case class PathElement(next: Option[IPathElement], link: Link)
    extends AbstractPathElement(next, link)
}
