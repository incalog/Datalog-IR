package org.inca.gp_old.constraints.element.virtual

import org.inca.core.constraints.IPathElement
import org.inca.core.constraints.element.virtual.IVirtualPathElement
import org.inca.gp_old.constraints.element.AbstractListPathElement
import org.inca.mps.InterfacePart

case class IndexPathElement(override var next: Option[IPathElement],
                            override var interfacePart: InterfacePart)
  extends AbstractListPathElement(next, interfacePart)
    with IVirtualPathElement
