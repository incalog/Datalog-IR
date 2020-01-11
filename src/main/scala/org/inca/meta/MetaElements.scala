package org.inca.meta

import java.lang.reflect.Field
import java.util.Objects

object MetaElements {

  trait MetaElement

  case class NodeType(cls: Class[_]) extends MetaElement {
    def apply(fieldName: String): NodeLink = NodeLink(this, cls.getDeclaredField(fieldName))

    override def toString: String = s"#${cls.getCanonicalName}"

    override def hashCode(): Int = cls.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case that: NodeType => cls.eq(that.cls)
      case _ => false
    }
  }

  case class DataType(cls: Class[_]) extends MetaElement {
    require(isPrimitiveDataType(cls), "Only primitive data types are allowed!")
  }

  def isPrimitiveDataType(cls: Class[_]): Boolean = {
    val isInteger = classOf[Int].isAssignableFrom(cls) || classOf[java.lang.Integer].isAssignableFrom(cls)
    val isBoolean = classOf[Boolean].isAssignableFrom(cls) || classOf[java.lang.Boolean].isAssignableFrom(cls)
    val isString = classOf[java.lang.String].isAssignableFrom(cls)
    isInteger || isBoolean || isString
  }

  case class NodeLink(nodeType: NodeType, fld: Field) extends MetaElement {
    override def toString: String = s"$nodeType:${fld.getName}"

    override def hashCode(): Int = Objects.hash(nodeType, fld)

    override def equals(obj: Any): Boolean = obj match {
      case that: NodeLink => nodeType.eq(that.nodeType) && fld.eq(that.fld)
      case _ => false
    }

  }

}
