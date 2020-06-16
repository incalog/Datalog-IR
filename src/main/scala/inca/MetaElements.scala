package inca

object MetaElements {

  case class NodeType(name: String) {
    def apply(fieldName: String): Link =
      fieldName match {
        case "parent" => ParentLink
        case "previous" => PreviousLink
        case "next" => NextLink
        case _ => NodeLink(this, fieldName)
      }
  }

  case class DataType(name: String) {
    require(isPrimitiveDataType(name), "Only primitive data types are allowed!")
  }

  def isPrimitiveDataType(cls: String): Boolean = {
    val primitiveNames = Seq(
      "java.lang.Integer",
      "java.lang.Long",
      "java.lang.Double",
      "java.lang.Boolean",
      "java.lang.String")
    primitiveNames.contains(cls)
  }

  trait Link {
    val nodeType: NodeType
    val fieldName: String
  }
  case class NodeLink(nodeType: NodeType, fieldName: String) extends Link
  case class DefinedNodeLink(nodeType: NodeType, fieldName: String) extends Link

  trait VirtualLink extends Link
  case object ParentLink extends VirtualLink {
    // TODO maybe better nodetype name? is fictional anyway
    override val nodeType: NodeType = NodeType("inca.lang.Node")
    override val fieldName: String = "parent"
  }
  case object PreviousLink extends VirtualLink {
    override val nodeType: NodeType = NodeType("inca.lang.Node")
    override val fieldName: String = "previous"
  }
  case object NextLink extends VirtualLink {
    override val nodeType: NodeType = NodeType("inca.lang.Node")
    override val fieldName: String = "next"
  }

}
