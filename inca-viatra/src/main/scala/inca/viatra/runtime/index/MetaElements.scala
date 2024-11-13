package inca.viatra.runtime.index

object MetaElements {
  type PrimitiveValue = Any

  /** A link consists of the name of the node's tag and the name of the link */
  //  type Link = (String, String)

  //  case class Link(tag: truechange.Tag, link: truechange.Link) {
  //    override def toString: String = "$tag.$link"
  //  }
  //
  //  trait Type {
  //    def name: String
  //    def mkKey: IndexKey[_]
  //  }
  //  case class PrimitiveType(name: String) extends Type {
  //    override def mkKey: PrimitiveTypeKey = PrimitiveTypeKey(this)
  //  }
  //
  //  def isIncaPrimitiveType(name: String): Boolean = {
  //    val javaPrimitives = Seq("java.lang.Integer", "java.lang.Long", "java.lang.Double", "java.lang.String", "java.lang.Boolean")
  //    javaPrimitives.contains(name)
  //  }
  //
  //  trait LinkedType extends Type {
  //    override def mkKey: NodeTypeKey = NodeTypeKey(this)
  //  }
  //  object LinkedType {
  //    def from(tag: truechange.Tag): LinkedType = tag match {
  //      case NamedTag(c) => NodeType(c)
  //      case ListTag(ty) => ListType(LinkedType.from(ty))
  //    }
  //
  //    def from(typ: truechange.Type): LinkedType = typ match {
  //      case NothingType =>
  //      case AnyType =>
  //      case SortType(tag) =>
  //      case truechange.ListType(ty) =>
  //      case OptionType(ty) =>
  //    }
  //  }
  //
  //  case class NodeType(name: String) extends LinkedType {
  //    def apply(field: String): Link =
  //      field match {
  //        case "parent" => ParentLink
  //        case "previous" => PreviousLink
  //        case "next" => NextLink
  //        case _ => NamedLink(truechange.NamedTag(name), truechange.NamedLink(field))
  //      }
  //  }
  //
  //  case class ListType(contained: LinkedType) extends LinkedType {
  //    val name: String = s"List[${contained.name}]"
  //
  //    def apply(field: String): Link = field match {
  //      case "parent" => ParentLink
  //      case "next" => NextLink
  //      case "previous" => PreviousLink
  //      case "first" => FirstLink(contained)
  //      case "elements" => ElementsLink(contained)
  //      case _ => throw new IllegalArgumentException(s"$this does not support named links $field")
  //    }
  //  }


  //  sealed trait Link {
  //    val typ: LinkedType
  //    val field: String
  //  }


  //  case class DefinedNodeLink(typ: LinkedType, field: String) extends Link
  //
  //  case object ParentLink extends Link {
  //    val typ: LinkedType = NodeType("inca.lang.Node")
  //    val field: String = "parent"
  //  }
  //
  //  case object NextLink extends Link {
  //    val typ: LinkedType = NodeType("inca.lang.Node")
  //    val field: String = "next"
  //  }
  //
  //  case object PreviousLink extends Link {
  //    val typ: LinkedType = NodeType("inca.lang.Node")
  //    val field: String = "previous"
  //  }
  //
  //  case class FirstLink(typ: LinkedType) extends Link {
  //    val field: String = "first"
  //  }
  //
  //  case class ElementsLink(typ: LinkedType) extends Link {
  //    val field: String = "elements"
  //  }
}
