package org.inca.lang.gp

import org.inca.lang.core.Constraints.IPathElement
import org.inca.lang.core.IVirtualPathElement
import org.inca.lang.gp.Element.AbstractListPathElement
import org.inca.lang.mps.Link

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
