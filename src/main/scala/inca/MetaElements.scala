package inca

//object MetaElements {
//  trait Type {
//    def name: String
//  }
//
//  trait Linked extends Type {
//    def apply(field: String): Link
//  }
//
//  case class Node(name: String) extends Linked {
//    def apply(field: String): Link = {
//      case "parent" => ParentLink
//      case "previous" => PreviousLink
//      case _ => NamedLink(this, name)
//    }
//  }
//
//  case class List(contained: Type) extends Linked {
//    val name: String = s"List[${contained.name}"
//
//    def apply(field: String): Link = field match {
//      case "first" => ListFirstLink(this)
//      case "next" => ListNextLink(this)
//      case _ => selectVirtualLink(field)
//    }
//  }
//
//  case class Primitive(name: String) extends Type
//
//
//  def selectVirtualLink(field: String): VirtualLink = field match {
//    case "parent" => ParentLink
//    case "previous" => PreviousLink
//    case _ => throw new IllegalArgumentException("Do not support virtual link " + field)
//  }
//
//  trait Link {
//    def field: String
//  }
//
//  case class NamedLink(node: Linked, field: String) extends Link
//
//  case class ListFirstLink(node: List) extends Link {
//    val field: String = "first"
//  }
//
//  case class ListNextLink() extends Link {
//    val field: String = "next"
//  }
//
//  trait VirtualLink extends Link
//  case object ParentLink extends VirtualLink {
//    val field: String = "parent"
//  }
//  case object PreviousLink extends VirtualLink {
//    val field: String = "previous"
//  }
//}
object MetaElements {
  trait Type {
    def name: String
  }
  case class Primitive(name: String) extends Type { /* require(isPrimitiveDataType(name), "Only primitive data types are allowed!")*/}
  def isPrimitiveDataType(name: String): Boolean = {
    val primitiveNames = Seq(
      "java.lang.Integer",
      "java.lang.Long",
      "java.lang.Double",
      "java.lang.Boolean",
      "java.lang.String")
    primitiveNames.contains(name)
  }

  trait Linked extends Type {
    def apply(field: String): Link
  }

  case class Node(name: String) extends Linked {
    def apply(field: String): Link =
      field match {
        case "parent" => ParentLink
        case "previous" => PreviousLink
        case "next" => ListNextLink()
        case _ => NamedLink(this, field)
      }
  }

  case class List(contained: Type) extends Linked {
    val name: String = s"List[${contained.name}"

    def apply(field: String): Link = field match {
      case "first" => ListFirstLink(this)
      case "next" => ListNextLink()
      case "parent" => ParentLink
      case "previous" => PreviousLink
      case _ => throw new IllegalArgumentException("Do not support link " + field)
    }
  }



  trait Link {
    val typ: Linked
    val field: String
  }

  case class NamedLink(typ: Linked, field: String) extends Link

  case class ListFirstLink(typ: List) extends Link {
    val field: String = "first"
  }
  case class ListNextLink() extends Link {
    val typ: Linked = Node("inca.lang.Node")
    val field: String = "next"
  }

  trait VirtualLink extends Link {
    val typ: Linked = Node("inca.lang.Node")
  }

  case object ParentLink extends VirtualLink {
    val field: String = "parent"
  }
  case object PreviousLink extends VirtualLink {
    val field: String = "previous"
  }

  case class DefinedNodeLink(typ: Linked, field: String) extends Link

//  trait VirtualLink extends Link
//  case object ParentLink extends VirtualLink {
//    // TODO maybe better nodetype name? is fictional anyway
//    override val typ: Node = Node("inca.lang.Node")
//    override val field: String = "parent"
//  }
//  case object PreviousLink extends VirtualLink {
//    override val typ: Node = Node("inca.lang.Node")
//    override val field: String = "previous"
//  }
//  case object NextLink extends VirtualLink {
//    override val typ: Node = Node("inca.lang.Node")
//    override val field: String = "next"
//  }

}
