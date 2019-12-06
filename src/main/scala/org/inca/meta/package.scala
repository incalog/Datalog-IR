package org.inca

import java.lang.reflect.Field

import org.inca.core.typ.ICompileTimeIncAType
import org.inca.mps.InterfacePart

package object meta {
  case class NodeType(cls: Class[_]) extends ICompileTimeIncAType {
    def apply(fieldName: String): NodeLink = NodeLink(this, cls.getDeclaredField(fieldName))

    override def toString: String = s"#${cls.getCanonicalName}"
  }
  case class NodeLink(nodeType: NodeType, fld: Field) extends InterfacePart {
    override def toString: String = s"$nodeType:${fld.getName}"
  }
}
