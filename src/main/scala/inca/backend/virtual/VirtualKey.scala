package inca.backend.virtual

import inca.MetaElements
import inca.backend.indices.TFInputKey

case class VirtualKey(val link: MetaElements.Link) extends TFInputKey[MetaElements.Link](link) {
  override def getArity = 2
}