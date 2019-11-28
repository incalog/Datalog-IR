package org.inca.gp.constraints.element

import org.inca.core.constraints.IPathElement
import org.inca.mps.InterfacePart

case class PathElement(override var next: Option[IPathElement],
                       override var interfacePart: InterfacePart)
  extends AbstractPathElement(next, interfacePart)
