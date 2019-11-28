package org.inca.gp.constraints.element

import org.inca.core.constraints.IPathElement
import org.inca.mps.InterfacePart

case class AbstractPathElement(next: Option[IPathElement],
                               interfacePart: InterfacePart)
  extends IPathElement