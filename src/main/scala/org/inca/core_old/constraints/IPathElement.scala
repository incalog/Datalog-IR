package org.inca.core_old.constraints

import org.inca.core_old.misc.ITransformable
import org.inca.mps.InterfacePart

trait IPathElement extends IPathElementScopeProvider with ITransformable {
  val next: Option[IPathElement]
  val interfacePart: InterfacePart
}
