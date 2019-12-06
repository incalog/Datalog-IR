package org.inca.gp_old.constraints.element

import org.inca.core.constraints.IPathElement
import org.inca.core.constraints.element.virtual.IVirtualPathElement
import org.inca.mps.InterfacePart

case class AbstractListPathElement(next: Option[IPathElement],
                                   interfacePart: InterfacePart)
  extends IPathElement
    with IVirtualPathElement
