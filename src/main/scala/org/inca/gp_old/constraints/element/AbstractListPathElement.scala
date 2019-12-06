package org.inca.gp_old.constraints.element

import org.inca.core_old.constraints.IPathElement
import org.inca.core_old.constraints.element.virtual.IVirtualPathElement
import org.inca.mps.InterfacePart

case class AbstractListPathElement(next: Option[IPathElement],
                                   interfacePart: InterfacePart)
  extends IPathElement
    with IVirtualPathElement
