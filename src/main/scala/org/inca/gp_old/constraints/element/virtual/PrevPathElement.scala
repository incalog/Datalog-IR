package org.inca.gp_old.constraints.element.virtual

import org.inca.core_old.constraints.IPathElement
import org.inca.core_old.constraints.element.virtual.IVirtualPathElement
import org.inca.gp_old.constraints.element.AbstractListPathElement
import org.inca.mps.InterfacePart

case class PrevPathElement(override var next: Option[IPathElement],
                           override var interfacePart: InterfacePart)
  extends AbstractListPathElement(next, interfacePart)
    with IVirtualPathElement