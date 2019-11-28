package org.inca.gp.constraints.element.gen

import org.inca.core.constraints.{IGeneratorPathElement, IPathElement}
import org.inca.gp.constraints.element.AbstractPathElement
import org.inca.mps.InterfacePart

case class GenListPathElement(isFirst: Boolean,
                              override var next: Option[IPathElement],
                              override var interfacePart: InterfacePart)
  extends AbstractPathElement(next, interfacePart)
    with IGeneratorPathElement
