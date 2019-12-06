package org.inca.gp

import org.inca.core.constraints.IPathElement
import org.inca.core.constraints.element.virtual.IVirtualPathElement
import org.inca.mps.InterfacePart

object Element {
  abstract class AbstractListPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends IPathElement with IVirtualPathElement
  abstract class AbstractPathElement(next: Option[IPathElement], interfacePart: InterfacePart) extends IPathElement

  case class PathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractPathElement(next, interfacePart)
}
