package org.inca.core.constraints

import org.inca.core.misc.ITransformable
import org.inca.mps.InterfacePart

trait IPathElement extends IPathElementScopeProvider with ITransformable {
  val next: Option[IPathElement]
  val interfacePart: InterfacePart
}
