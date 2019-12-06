package org.inca.gp_old.constraints.element

import org.inca.core_old.constraints.IPathElement
import org.inca.mps.InterfacePart

case class PathElement(override var next: Option[IPathElement],
                       override var interfacePart: InterfacePart)
  extends AbstractPathElement(next, interfacePart)
