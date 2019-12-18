package org.inca

import java.lang.reflect.Field

import org.inca.lang.core.Typp.Typ
import org.inca.lang.mps.Link

package object meta {
  case class NodeType(cls: Class[_]) extends Typ {
    def apply(fieldName: String): NodeLink = NodeLink(this, cls.getDeclaredField(fieldName))

    override def toString: String = s"#${cls.getCanonicalName}"
  }
  case class NodeLink(nodeType: NodeType, fld: Field) extends Link {
    override def toString: String = s"$nodeType:${fld.getName}"
  }
}
