package org.inca.lang.gp

import org.inca.lang.core.Constraints.IPathElement
import org.inca.lang.core.IVirtualPathElement
import org.inca.lang.mps.Link

object Element {
  abstract class AbstractListPathElement(next: Option[IPathElement], link: Link)
    extends IPathElement with IVirtualPathElement

  abstract class AbstractPathElement(next: Option[IPathElement], link: Link) extends IPathElement


  case class PathElement(next: Option[IPathElement], link: Link)
    extends AbstractPathElement(next, link)
}
