package org.inca.gp_old.constraints.element.gen

import org.inca.core_old.constraints.{IGeneratorPathElement, IPathElement}
import org.inca.gp_old.constraints.element.AbstractPathElement
import org.inca.mps.InterfacePart

case class GenListPathElement(isFirst: Boolean,
                              override var next: Option[IPathElement],
                              override var interfacePart: InterfacePart)
  extends AbstractPathElement(next, interfacePart)
    with IGeneratorPathElement
