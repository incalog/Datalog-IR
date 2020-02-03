package org.inca.lang

import java.lang.reflect.Field

import org.inca.lang.core.Typp.Typ

package object meta {
  trait Link

  case class NodeType(cls: Class[_]) extends Typ {

    def apply(fieldName: String): Link =
      fieldName match {
        case "parent" => ParentLink()
        case "previous" => PreviousLink()
        case "next" => NextLink()

        case _ => NodeLink(this, cls.getDeclaredField(fieldName))
      }

    override def toString: String = s"${cls.getCanonicalName}"
  }

  case class NodeLink(nodeType: NodeType, fld: Field) extends Link {
    override def toString: String = s"${fld.getName}"
  }

  case class ParentLink() extends Link {
    override def toString: String = "parent"
  }
  case class PreviousLink() extends Link {
    override def toString: String = "previous"
  }
  case class NextLink() extends Link {
    override def toString: String = "next"
  }
}
