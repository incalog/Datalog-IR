package org.inca.core.constraints

import org.inca.core.misc.ITransformable
import org.inca.mps.InterfacePart

trait IPathElement extends ITransformable {
  var next: Option[IPathElement]
  var interfacePart: InterfacePart
}
