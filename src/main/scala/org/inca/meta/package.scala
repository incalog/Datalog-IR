package org.inca

import java.lang.reflect.Field

package object meta {
  trait IType
  case class NodeType(cls: Class[_]) extends IType {
    def apply(fieldName: String): NodeLink = NodeLink(this, cls.getDeclaredField(fieldName))

    override def toString: String = s"#${cls.getCanonicalName}"
  }


  trait ILink
  case class NodeLink(nodeType: NodeType, fld: Field) extends ILink {
    override def toString: String = s"$nodeType:${fld.getName}"
  }
}
