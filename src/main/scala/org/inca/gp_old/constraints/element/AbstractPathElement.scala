package org.inca.gp_old.constraints.element

import org.inca.core_old.constraints.IPathElement
import org.inca.mps.InterfacePart

case class AbstractPathElement(next: Option[IPathElement],
                               interfacePart: InterfacePart)
  extends IPathElement