package inca.runtime.context

import inca.runtime.index.MetaElements._

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
class LanguageMetaInfo(_directSupertypes: Map[NodeType, Set[NodeType]], _links: Map[NamedLink, Type]) {
  def this() = this(Map(), Map())

  val links: Map[NamedLink, Type] = _links

  val directNodeSupertypes: Map[NodeType, Set[NodeType]] = _directSupertypes
  val directNodeSubtypes: Map[NodeType, Set[NodeType]] = {
    val res = mutable.Map[NodeType, Set[NodeType]]()
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

  val nodeSupertypes: Map[NodeType, Set[NodeType]] = transClosure(_directSupertypes)
  val nodeSubtypes: Map[NodeType, Set[NodeType]] = transClosure(directNodeSubtypes)


  def directSupertypes(ty: LinkedType): Iterable[LinkedType] = ty match {
    case ty: NodeType => directNodeSupertypes.getOrElse(ty, Iterable())
    case ListType(contained) => directSupertypes(contained).map(ListType)
  }
  def directSubtypes(ty: LinkedType): Iterable[LinkedType] = ty match {
    case ty: NodeType => directNodeSubtypes.getOrElse(ty, Iterable())
    case ListType(contained) => directSubtypes(contained).map(ListType)
  }

  def supertypes(ty: LinkedType): Iterable[LinkedType] = ty match {
    case ty: NodeType => nodeSupertypes.getOrElse(ty, Iterable())
    case ListType(contained) => supertypes(contained).map(ListType)
  }
  def subtypes(ty: LinkedType): Iterable[LinkedType] = ty match {
    case ty: NodeType => nodeSubtypes.getOrElse(ty, Iterable())
    case ListType(contained) => subtypes(contained).map(ListType)
  }

  @scala.annotation.tailrec
  private def transClosure(rel: Map[NodeType, Set[NodeType]]): Map[NodeType, Set[NodeType]] = {
    val newRel = rel.map { case (src, trg) =>
      src -> (trg ++ trg.flatMap { s => rel(s) })
    }
    if (newRel == rel) rel
    else transClosure(newRel)
  }
}
