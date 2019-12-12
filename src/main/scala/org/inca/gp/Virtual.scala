package org.inca.gp

import org.inca.core.Constraints.IPathElement
import org.inca.core.IVirtualPathElement
import org.inca.gp.Element.AbstractListPathElement
import org.inca.mps.Link

object Virtual {
  case class IndexPathElement(next: Option[IPathElement], link: Link)
    extends AbstractListPathElement(next, link) with IVirtualPathElement
  case class NextPathElement(next: Option[IPathElement], link: Link)
    extends AbstractListPathElement(next, link) with IVirtualPathElement
  case class ParentPathElement(next: Option[IPathElement], link: Link)
    extends AbstractListPathElement(next, link) with IVirtualPathElement
  case class PrevPathElement(next: Option[IPathElement], link: Link)
    extends AbstractListPathElement(next, link) with IVirtualPathElement
}
