package inca

import java.lang.reflect.Field

object MetaElements {


  case class NodeType(cls: Class[_]) {
    def apply(fieldName: String): Link =
      fieldName match {
        case "parent" => ParentLink()
        case "previous" => PreviousLink()
        case "next" => NextLink()

        case _ => NodeLink(this, cls.getDeclaredField(fieldName))
      }
  }

  case class DataType(cls: Class[_]) {
    require(isPrimitiveDataType(cls), "Only primitive data types are allowed!")
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

  trait Link {
    val nodeType: NodeType
    val fld: Field
  }

  case class NodeLink(nodeType: NodeType, fld: Field) extends Link

  case class DefinedNodeLink(nodeType: NodeType, fld: Field) extends Link

  case class Node(parent: Option[Node],
                  previous: Option[Node],
                  next: Option[Node])

  case class ParentLink() extends Link {
    override val nodeType: NodeType = NodeType(classOf[Node])
    override val fld: Field = classOf[Node].getDeclaredField("parent")
  }
  case class PreviousLink() extends Link {
    override val nodeType: NodeType = NodeType(classOf[Node])
    override val fld: Field = classOf[Node].getDeclaredField("previous")
  }
  case class NextLink() extends Link {
    override val nodeType: NodeType = NodeType(classOf[Node])
    override val fld: Field = classOf[Node].getDeclaredField("next")
  }

}
