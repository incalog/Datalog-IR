package inca.backend.indices

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
class LanguageMetaInfo(_directSupertypes: Map[String, Set[String]], _links: Map[String, Map[String, String]]) {
  val directSupertypes: Map[String, Set[String]] = _directSupertypes
  val links: Map[String, Map[String, String]] = _links

  val supertypes: Map[String, Set[String]] = transClosure(_directSupertypes)

  val directSubtypes: Map[String, Set[String]] = {
    val res = mutable.Map[String, Set[String]]()
    // initialize
    directSupertypes.foreach { case (ty, supers) =>
      res(ty) = Set()
      supers.foreach { sty =>
        res(sty) = Set()
      }
    }
    directSupertypes.foreach { case (ty, supers) =>
      supers.foreach { sty =>
        res(sty) = res(sty) + ty
      }
    }
    Map() ++ res
  }

  val subtypes: Map[String, Set[String]] = transClosure(directSubtypes)

  private def transClosure(rel: Map[String, Set[String]]): Map[String, Set[String]] = {
    val newRel = rel.map { case (src, trg) =>
      src -> (trg ++ trg.flatMap { s => rel(s) })
    }
    if (newRel == rel) rel
    else transClosure(newRel)
  }
}
