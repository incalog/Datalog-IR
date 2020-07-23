package inca.runtime

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
        case "next" => NextLink
        case _ => NamedLink(this, field)
      }
  }

  case class ListType(contained: LinkedType) extends LinkedType {
    val name: String = s"List[${contained.name}]"

    def apply(field: String): Link = field match {
      case "parent" => ParentLink
      case "next" => NextLink
      case "previous" => PreviousLink
      case "first" => FirstLink(contained)
      case "elements" => ElementsLink(contained)
      case _ => throw new IllegalArgumentException(s"$this does not support named links $field")
    }
  }


  sealed trait Link {
    val typ: LinkedType
    val field: String
  }

  case class NamedLink(typ: NodeType, field: String) extends Link

  case class DefinedNodeLink(typ: LinkedType, field: String) extends Link

  case object ParentLink extends Link {
    val typ: LinkedType = NodeType("inca.lang.Node")
    val field: String = "parent"
  }

  case object NextLink extends Link {
    val typ: LinkedType = NodeType("inca.lang.Node")
    val field: String = "next"
  }

  case object PreviousLink extends Link {
    val typ: LinkedType = NodeType("inca.lang.Node")
    val field: String = "previous"
  }

  case class FirstLink(typ: LinkedType) extends Link {
    val field: String = "first"
  }

  case class ElementsLink(typ: LinkedType) extends Link {
    val field: String = "elements"
  }
}
