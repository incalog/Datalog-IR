package inca.runtime.context

import inca.runtime.context.DataModel.Link
import inca.util.TupleOps.transClosure
import truechange.{Link => _, _}

import scala.collection.immutable.MultiDict

/**
 * This class captures meta information about a data model definition.
 * - directSupertypes maps a type name to all names of types that are the direct supertypes.
 * - supertypes represents the transitive closure of directSupertypes.
 * - links maps a link to its target type
 * - litLinks maps a link to its target lit type
 * - directSubtypes maps a type name to all names of types that are the direct subtypes.
 * - subtypes represents the transitive closure of directSubtypes.
 */
class DataModel(
                        val types: Set[SortType],
                        _directSupertypes: MultiDict[SortType, SortType],
                        val links: Map[Link, Type],
                        val litLinks: Map[Link, LitType]
                      ) {

  override def toString: String = {
    s"DataModel($types, $directNodeSupertypes, $links, $litLinks)"
  }

  def this() = this(Set(), MultiDict(), Map(), Map())
  def ++(other: DataModel): DataModel =
    new DataModel(
      this.types ++ other.types,
      this.directNodeSupertypes.concat(other.directNodeSupertypes),
      this.links ++ other.links,
      this.litLinks ++ other.litLinks
    )

  val directNodeSupertypes: MultiDict[SortType, SortType] = _directSupertypes
  lazy val directNodeSubtypes: MultiDict[SortType, SortType] = {
    var res = MultiDict[SortType, SortType]()
    directNodeSupertypes.foreach { case (ty, sty) =>
      res += sty -> ty
    }
    res
  }

  def isSubtype(sub: SortType, sup: SortType): Boolean =
    nodeSupertypes.containsEntry(sub -> sup)

  /** maps subtype to supertypes */
  lazy val nodeSupertypes: MultiDict[SortType, SortType] = transClosure(directNodeSupertypes)
  /** maps supertype to subtypes */
  lazy val nodeSubtypes: MultiDict[SortType, SortType] = transClosure(directNodeSubtypes)


  def directSupertypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => directNodeSupertypes.get(ty) ++ Seq(AnyType)
    case ListType(contained) => directSupertypes(contained).map(ListType) ++ Seq(AnyType)
    case OptionType(contained) => directSupertypes(contained).map(OptionType) ++ Seq(AnyType)
    case RefType(contained) => directSupertypes(contained).map(RefType) ++ Seq(AnyType)
    case AnyType => Iterable()
    case NothingType => throw new UnsupportedOperationException("The supertypes of NothingType are not enumerable")
  }
  def directSubtypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => directNodeSubtypes.get(ty) ++ Seq(NothingType)
    case ListType(contained) => directSubtypes(contained).map(ListType) ++ Seq(NothingType)
    case OptionType(contained) => directSubtypes(contained).map(OptionType) ++ Seq(NothingType)
    case RefType(contained) => directSubtypes(contained).map(RefType) ++ Seq(AnyType)
    case AnyType => throw new UnsupportedOperationException("The subtypes of AnyType are not enumerable")
    case NothingType => Iterable()
  }

  def supertypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => nodeSupertypes.get(ty) ++ Seq(AnyType)
    case ListType(contained) => supertypes(contained).map(ListType) ++ Seq(AnyType)
    case OptionType(contained) => supertypes(contained).map(OptionType) ++ Seq(AnyType)
    case RefType(contained) => supertypes(contained).map(RefType) ++ Seq(AnyType)
    case AnyType => Iterable()
    case NothingType => throw new UnsupportedOperationException("The supertypes of NothingType are not enumerable")
  }
  def subtypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => nodeSubtypes.get(ty) ++ Seq(NothingType)
    case ListType(contained) => subtypes(contained).map(ListType) ++ Seq(NothingType)
    case OptionType(contained) => subtypes(contained).map(OptionType) ++ Seq(NothingType)
    case RefType(contained) => subtypes(contained).map(RefType) ++ Seq(NothingType)
    case AnyType => throw new UnsupportedOperationException("The subtypes of AnyType are not enumerable")
    case NothingType => Iterable()
  }
}

object DataModel {
  type Link = (String, String)

  def from(nodes: NodeMetaInfo*): DataModel = {
    val types = nodes.map(n => n.sort).toSet
    val _directSupertypes: MultiDict[SortType, SortType] =
      MultiDict.from(nodes.flatMap(n => n.superSorts.map(sup => n.sort -> sup)))
    val links: Map[Link, Type] = Map.from(nodes.flatMap(n => n.links.map {
      case (NamedLink(name), ty) => (n.sort.name, name) -> ty
    }))
    val litLinks: Map[Link, LitType] = Map.from(nodes.flatMap(n => n.litLinks.map {
      case (NamedLink(name), ty) => (n.sort.name, name) -> ty
    }))
    new DataModel(types, _directSupertypes, links, litLinks)
  }
}