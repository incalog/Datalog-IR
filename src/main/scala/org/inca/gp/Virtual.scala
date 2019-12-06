package org.inca.gp

import org.inca.core_old.constraints.IPathElement
import org.inca.core_old.constraints.element.virtual.IVirtualPathElement
import org.inca.gp.Element.AbstractListPathElement
import org.inca.mps.InterfacePart

object Virtual {
  case class FirstPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractListPathElement(next, interfacePart)
  case class IndexPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractListPathElement(next, interfacePart) with IVirtualPathElement
  case class LastPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractListPathElement(next, interfacePart)
  case class NextPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractListPathElement(next, interfacePart) with IVirtualPathElement
  case class ParentPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractListPathElement(next, interfacePart) with IVirtualPathElement
  case class PrevPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractListPathElement(next, interfacePart) with IVirtualPathElement
}
