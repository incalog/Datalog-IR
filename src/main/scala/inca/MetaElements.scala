package inca

import java.lang.reflect.Field
import java.util.Objects

object MetaElements {

  trait MetaElement
  trait Link extends MetaElement {
    val nodeType: NodeType
    val fld: Field
  }

  case class NodeType(cls: Class[_]) extends MetaElement {
    def apply(fieldName: String): Link =
      fieldName match {
        case "parent" => ParentLink()
        case "previous" => PreviousLink()
        case "next" => NextLink()

        case _ => NodeLink(this, cls.getDeclaredField(fieldName))
      }

    override def toString: String = s"#${cls.getCanonicalName}"

    override def hashCode(): Int = cls.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case that: NodeType => cls.equals(that.cls)
      case _ => false
    }
  }

  case class DataType(cls: Class[_]) extends MetaElement {
    require(isPrimitiveDataType(cls), "Only primitive data types are allowed!")

    override def toString: String = s"#${cls.getCanonicalName}"

    override def hashCode(): Int = cls.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case that: DataType => cls.equals(that.cls)
      case _ => false
    }
  }

  def isPrimitiveDataType(cls: Class[_]): Boolean = {
    val isNumber = classOf[Int].isAssignableFrom(cls) ||
      classOf[java.lang.Integer].isAssignableFrom(cls) ||
      classOf[Long].isAssignableFrom(cls) ||
      classOf[java.lang.Long].isAssignableFrom(cls) ||
      classOf[Double].isAssignableFrom(cls) ||
      classOf[java.lang.Double].isAssignableFrom(cls) ||
      classOf[AnyVal].isAssignableFrom(cls)
    val isBoolean = classOf[Boolean].isAssignableFrom(cls) ||
      classOf[java.lang.Boolean].isAssignableFrom(cls)
    val isString = classOf[String].isAssignableFrom(cls) ||
      classOf[java.lang.String].isAssignableFrom(cls)

    isNumber || isBoolean || isString
  }

  case class NodeLink(nodeType: NodeType, fld: Field) extends Link {
    override def toString: String = s"$nodeType:${fld.getName}"

    override def hashCode(): Int = Objects.hash(nodeType, fld)

    override def equals(obj: Any): Boolean = obj match {
      case that: NodeLink => nodeType.equals(that.nodeType) && fld.equals(that.fld)
      case _ => false
    }
  }

  case class DefinedNodeLink(nodeType: NodeType, fld: Field) extends Link {
    override def toString: String = s"$nodeType:${fld.getName}_isDefinied"

    override def hashCode(): Int = Objects.hash(nodeType, fld)

    override def equals(obj: Any): Boolean = obj match {
      case that: DefinedNodeLink => nodeType.equals(that.nodeType) && fld.equals(that.fld)
      case _ => false
    }
  }

  case class Node(parent: Option[Node],
                  previous: Option[Node],
                  next: Option[Node])

  case class ParentLink() extends Link {
    override def toString: String = "parent"

    override val nodeType: NodeType = NodeType(classOf[Node])
    override val fld: Field = classOf[Node].getDeclaredField(toString)
  }
  case class PreviousLink() extends Link {
    override def toString: String = "previous"

    override val nodeType: NodeType = NodeType(classOf[Node])
    override val fld: Field = classOf[Node].getDeclaredField(toString)
  }
  case class NextLink() extends Link {
    override def toString: String = "next"

    override val nodeType: NodeType = NodeType(classOf[Node])
    override val fld: Field = classOf[Node].getDeclaredField(toString)
  }

}
