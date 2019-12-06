package org.inca.gp

import org.inca.core_old.constraints.{IGeneratorPathElement, IPathElement}
import org.inca.gp.Element.AbstractPathElement
import org.inca.mps.InterfacePart

object Gen {
  case class GenListPathElement(isFirst: Boolean, next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractPathElement(next, interfacePart) with IGeneratorPathElement

  case class GenIsDefinedPathElement(next: Option[IPathElement], interfacePart: InterfacePart)
    extends AbstractPathElement(next, interfacePart) with IGeneratorPathElement
}
