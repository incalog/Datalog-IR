package inca.ir

import inca.ir.typing.Mode
import inca.ir.{Atom, Body, Eq, Name, RefByName, Term, TermType, Var}
import inca.ir.visitors.IRVisitor

/**
 * TODO: We need to restructure the optimizations (including the once with the abstract interpreter)
 *  into a better package structure
 *
 * Note: We currently need this because the DependencyAnalysis becomes to big. We need to make the
 * file size smaller by removing aliases.
 *
 * For now this optimization eliminates simple variables aliases, where both the left
 * and right hand side are variables.
 * Only execute this after all other lowerings have been applied.
 *
 * E.g
 *
 * b == 4
 * a == b
 * c == a
 * R(c)
 *
 * ~>
 * 
 * b == 4
 * R(b)
 *
 */

trait SimpleAliasElimination extends IRVisitor:
  override def name: String = "SimpleAliasElimination"

  private var aliases: Map[Name, Name] = Map()
  private var params: Set[Name] = Set()

  protected def scoped[A](f: => A): A =
    val oldAliases = aliases
    try {
      val a = f
      a
    } finally {
      aliases = oldAliases
    }

  private def addAlias(alias: Name, target: Name): Unit =
    lookupAlias(target) match
      case Some(newTarget) => aliases += alias -> newTarget
      case _ => aliases += alias -> target

  private def lookupAlias(alias: Name): Option[Name] =
    aliases.get(alias)

  private def isParam(name: Name): Boolean =
    params.contains(name)

  override def visitRelation(relation: Relation): Seq[Relation] = scoped {
    params = relation.params.map(_.name).toSet
    super.visitRelation(relation)
  }

  override def visitBody(body: Body): Seq[Body] =
    scoped(super.visitBody(body))

  override def visitTerm(term: Term): Seq[Term] = term match
    case v: Var =>
      lookupAlias(v.name) match
        case Some(name) => Seq(Var(RefByName(name)))
        case _ => super.visitTerm(v)
    case _ => super.visitTerm(term)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Eq(v1: Var, v2: Var, false) =>
      (v1.typ, v2.typ) match
        case (Some(TermType(_, Mode.Binding)), Some(TermType(_, Mode.Bound))) if !isParam(v1.name) =>
          addAlias(v1.name, v2.name)
          Seq()
        case (Some(TermType(_, Mode.Bound)), Some(TermType(_, Mode.Binding))) if !isParam(v2.name) =>
          addAlias(v2.name, v1.name)
          Seq()
        case _ =>
          super.visitAtom(atom)
    case _ => super.visitAtom(atom)