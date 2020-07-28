package inca.runtime.context

import inca.runtime.index.MetaElements._
import truechange.{Link => _, _}

import scala.collection.mutable

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
                        _directSupertypes: Map[SortType, Set[SortType]],
                        val links: Map[Link, Type],
                        val litLinks: Map[Link, LitType]
                      ) {

  def this() = this(Map(), Map(), Map())

  val directNodeSupertypes: Map[SortType, Set[SortType]] = _directSupertypes
  val directNodeSubtypes: Map[SortType, Set[SortType]] = {
    val res = mutable.Map[SortType, Set[SortType]]()
    // initialize
    directNodeSupertypes.foreach { case (ty, supers) =>
      res(ty) = Set()
      supers.foreach { sty =>
        res(sty) = Set()
      }
    }
    directNodeSupertypes.foreach { case (ty, supers) =>
      supers.foreach { sty =>
        res(sty) = res(sty) + ty
      }
    }
    res.toMap
  }

  val nodeSupertypes: Map[SortType, Set[SortType]] = transClosure(directNodeSupertypes)
  val nodeSubtypes: Map[SortType, Set[SortType]] = transClosure(directNodeSubtypes)


  def directSupertypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => directNodeSupertypes.getOrElse(ty, Iterable()) ++ Seq(AnyType)
    case ListType(contained) => directSupertypes(contained).map(ListType) ++ Seq(AnyType)
    case OptionType(contained) => directSupertypes(contained).map(OptionType) ++ Seq(AnyType)
    case AnyType => Iterable()
    case NothingType => throw new UnsupportedOperationException("The supertypes of NothingType are not enumerable")
  }
  def directSubtypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => directNodeSubtypes.getOrElse(ty, Iterable()) ++ Seq(NothingType)
    case ListType(contained) => directSubtypes(contained).map(ListType) ++ Seq(NothingType)
    case OptionType(contained) => directSubtypes(contained).map(OptionType) ++ Seq(NothingType)
    case AnyType => throw new UnsupportedOperationException("The supertypes of AnyType are not enumerable")
    case NothingType => Iterable()
  }

  def supertypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => nodeSupertypes.getOrElse(ty, Iterable()) ++ Seq(AnyType)
    case ListType(contained) => supertypes(contained).map(ListType) ++ Seq(AnyType)
    case OptionType(contained) => supertypes(contained).map(OptionType) ++ Seq(AnyType)
    case AnyType => Iterable()
    case NothingType => throw new UnsupportedOperationException("The supertypes of NothingType are not enumerable")
  }
  def subtypes(ty: Type): Iterable[Type] = ty match {
    case ty: SortType => nodeSubtypes.getOrElse(ty, Iterable()) ++ Seq(NothingType)
    case ListType(contained) => subtypes(contained).map(ListType) ++ Seq(NothingType)
    case OptionType(contained) => subtypes(contained).map(OptionType) ++ Seq(NothingType)
    case AnyType => throw new UnsupportedOperationException("The supertypes of AnyType are not enumerable")
    case NothingType => Iterable()
  }

  @scala.annotation.tailrec
  private def transClosure(rel: Map[SortType, Set[SortType]]): Map[SortType, Set[SortType]] = {
    val newRel = rel.map { case (src, trg) =>
      src -> (trg ++ trg.flatMap { s => rel(s) } )
    }
    if (newRel == rel) rel
    else transClosure(newRel)
  }
}
