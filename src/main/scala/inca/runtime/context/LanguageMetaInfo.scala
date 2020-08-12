package inca.runtime.context

import inca.runtime.index.MetaElements._
import truechange.{Link => _, _}

import scala.collection.immutable.MultiDict

/**
 * This class captures meta information about a language definition.
 * - directSupertypes maps a type name to all names of types that are the direct supertypes.
 * - supertypes represents the transitive closure of directSupertypes.
 * - links maps a type name to a map that has an entry for each link name indicating what the target type name is.
 * - directSubtypes maps a type name to all names of types that are the direct subtypes.
 * - subtypes represents the transitive closure of directSubtypes.
 *
 * For the case class representing the abstract syntax of Add:
 *   case class Add(lhs: Exp, rhs: Exp) extends Exp
 * links contains ("Add" -> Map("lhs" -> "Exp", "rhs" -> "Exp"))
 */
// TODO add what the node types and what primitives are
// node types are the keys of directSupertypes
class LanguageMetaInfo(
                        _directSupertypes: MultiDict[SortType, SortType],
                        val links: Map[Link, Type],
                        val litLinks: Map[Link, LitType]
                      ) {

  def this() = this(MultiDict(), Map(), Map())

  val directNodeSupertypes: MultiDict[SortType, SortType] = _directSupertypes
  val directNodeSubtypes: MultiDict[SortType, SortType] = {
    var res = MultiDict[SortType, SortType]()
    directNodeSupertypes.foreach { case (ty, sty) =>
      res += sty -> ty
    }
    res
  }

  /** maps subtype to supertypes */
  val nodeSupertypes: MultiDict[SortType, SortType] = transClosure(directNodeSupertypes)
  /** maps supertype to subtypes */
  val nodeSubtypes: MultiDict[SortType, SortType] = transClosure(directNodeSubtypes)


  def directSupertypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => directNodeSupertypes.get(ty) ++ Seq(AnyType)
    case ListType(contained) => directSupertypes(contained).map(ListType) ++ Seq(AnyType)
    case OptionType(contained) => directSupertypes(contained).map(OptionType) ++ Seq(AnyType)
    case AnyType => Iterable()
    case NothingType => throw new UnsupportedOperationException("The supertypes of NothingType are not enumerable")
  }
  def directSubtypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => directNodeSubtypes.get(ty) ++ Seq(NothingType)
    case ListType(contained) => directSubtypes(contained).map(ListType) ++ Seq(NothingType)
    case OptionType(contained) => directSubtypes(contained).map(OptionType) ++ Seq(NothingType)
    case AnyType => throw new UnsupportedOperationException("The supertypes of AnyType are not enumerable")
    case NothingType => Iterable()
  }

  def supertypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => nodeSupertypes.get(ty) ++ Seq(AnyType)
    case ListType(contained) => supertypes(contained).map(ListType) ++ Seq(AnyType)
    case OptionType(contained) => supertypes(contained).map(OptionType) ++ Seq(AnyType)
    case AnyType => Iterable()
    case NothingType => throw new UnsupportedOperationException("The supertypes of NothingType are not enumerable")
  }
  def subtypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => nodeSubtypes.get(ty) ++ Seq(NothingType)
    case ListType(contained) => subtypes(contained).map(ListType) ++ Seq(NothingType)
    case OptionType(contained) => subtypes(contained).map(OptionType) ++ Seq(NothingType)
    case AnyType => throw new UnsupportedOperationException("The supertypes of AnyType are not enumerable")
    case NothingType => Iterable()
  }

  @scala.annotation.tailrec
  private def transClosure(rel: MultiDict[SortType, SortType]): MultiDict[SortType, SortType] = {
    val newRel = rel.mapSets { case (src, trg) =>
      src -> (trg ++ trg.flatMap { s => rel.get(s) } )
    }
    if (newRel == rel) rel
    else transClosure(newRel)
  }
}
