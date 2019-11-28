package org.inca.gp.constraints.element.virtual

import org.inca.core.constraints.IPathElement
import org.inca.core.constraints.element.virtual.IVirtualPathElement
import org.inca.gp.constraints.element.AbstractListPathElement
import org.inca.mps.InterfacePart

case class ParentPathElement(override var next: Option[IPathElement],
                             override var interfacePart: InterfacePart)
  extends AbstractListPathElement(next, interfacePart)
    with IVirtualPathElement
