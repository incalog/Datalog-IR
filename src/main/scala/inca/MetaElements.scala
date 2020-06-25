package inca

object MetaElements {
  trait Type {
    def name: String
  }

  case class PrimitiveType(name: String) extends Type

  def isIncaPrimitiveType(name: String): Boolean = {
    val javaPrimitives = Seq("java.lang.Integer", "java.lang.Long", "java.lang.Double", "java.lang.String", "java.lang.Boolean")
    javaPrimitives.contains(name)
  }

  trait LinkedType extends Type {
    def apply(field: String): Link
  }

  case class NodeType(name: String) extends LinkedType {
    def apply(field: String): Link =
      field match {
        case "parent" => ParentLink
        case "previous" => PreviousLink
        case "next" => ListNextLink()
        case _ => NamedLink(this, field)
      }
  }

  case class ListType(contained: Type) extends LinkedType {
    val name: String = s"List[${contained.name}"

    def apply(field: String): Link = field match {
      case "first" => ListFirstLink(this)
      case "next" => ListNextLink()
      case "elements" => ListElementsLink()
      case "parent" => ParentLink
      case "previous" => PreviousLink
      case _ => throw new IllegalArgumentException("Do not support link " + field)
    }
  }


  sealed trait Link {
    val typ: LinkedType
    val field: String
  }

  case class NamedLink(typ: NodeType, field: String) extends Link

  case class ListFirstLink(typ: ListType) extends Link {
    val field: String = "first"
  }

  case class ListNextLink() extends Link {
    val typ: LinkedType = NodeType("inca.lang.Node")
    val field: String = "next"
  }

  case class DefinedNodeLink(typ: LinkedType, field: String) extends Link

  trait VirtualLink extends Link {
    val typ: LinkedType = NodeType("inca.lang.Node")
  }

  case object ParentLink extends VirtualLink {
    val field: String = "parent"
  }
  case object PreviousLink extends VirtualLink {
    val field: String = "previous"
  }
  case class ListElementsLink() extends VirtualLink {
    val field: String = "elements"
  }
}
