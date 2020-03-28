package org.inca.lang.gp

import org.inca.lang.core.Constraints.IPathElement
import org.inca.lang.core.IVirtualPathElement
import org.inca.meta.MetaElements.Link

object Virtual {

  case class NextPathElement(next: Option[IPathElement], link: Link) extends IVirtualPathElement

  case class ParentPathElement(next: Option[IPathElement], link: Link) extends IVirtualPathElement

  case class PrevPathElement(next: Option[IPathElement], link: Link) extends IVirtualPathElement

}
