package inca.ir

import inca.ir.typing.Mode
import inca.ir.{Atom, Body, Eq, Name, RefByName, Term, TermType, Var}
import inca.ir.visitors.IRVisitor

/**
 * TODO: We need to restructure the optimizations (including the one with the abstract interpreter)
 *  into a better package structure
 */

trait AliasElimination extends IRVisitor:
  case class BodyMustFail(atom: Atom) extends Exception

  override def name: String = "AliasElimination"

  enum Phase:
    // Remove simple variable aliases (Note: this ignores equalities that contain a parameter)
    //   e.g. a == 1, b: >< == a: <>, c: >< == b: <>, R(c)  ~>  a == 1, R(a)
    case RemoveVariableAliases
    // Replace variables with a parameter, if a corresponding equality exists
    //   e.g. Q(x) :- a: >< == 1, x: >< == a: <>  ~>  Q(x) :- x == 1
    case ReplaceVariablesByParameters
    // Remove equalities where lhs == rhs or lhs != rhs, but lhs is the same var as rhs
    //  e.g. Q(a) :- a == a, R(a)  ~>  R(a)
    //  e.g. Q(a) :- a != a, R(a)  ~>  Q(a) :- .
    case SimplifyEqualities

  private var phase: Phase = _

  private var paramAliases: Map[Name, Name] = Map()
  private var aliases: Map[Name, Name] = Map()
  private var params: Set[Name] = Set()

  protected def scoped[A](f: => A): A =
    val oldAliases = aliases
    val oldParamAlias = paramAliases
    try {
      val a = f
      a
    } finally {
      aliases = oldAliases
      paramAliases = oldParamAlias
    }

  private def addAlias(alias: Name, target: Name): Unit =
    aliases.get(target) match
      case Some(newTarget) => aliases += alias -> newTarget
      case _ => aliases += alias -> target

  private def addParameterAlias(alias: Name, param: Name): Unit =
    paramAliases.get(alias) match
      case Some(newTarget) => // nothing
      case _ => paramAliases += alias -> param

  private def lookupAlias(alias: Name): Option[Name] = phase match
    case Phase.RemoveVariableAliases =>
      aliases.get(alias)
    case Phase.ReplaceVariablesByParameters =>
      val resolvedAlias = aliases.getOrElse(alias, alias)
      paramAliases.get(resolvedAlias)
    case _ => None

  private def isParam(name: Name): Boolean =
    params.contains(name)

  override def visitRelation(relation: Relation): Seq[Relation] = scoped {
    params = relation.params.map(_.name).toSet
    val rels = super.visitRelation(relation)
    params = Set()
    rels
  }

  override def visitBody(body: Body): Seq[Body] =
    scoped {
      phase = Phase.RemoveVariableAliases
      val Seq(b1) = super.visitBody(body)
      phase = Phase.ReplaceVariablesByParameters
      val Seq(b2) = super.visitBody(b1)
      phase = Phase.SimplifyEqualities

      try {
        super.visitBody(b2)
      } catch {
        case BodyMustFail(_) => Seq()
      }
    }

  override def visitTerm(term: Term): Seq[Term] = term match
    case v: Var => lookupAlias(v.name) match
      case Some(name) => Seq(Var(RefByName(name)))
      case _ => super.visitTerm(v)
    case _ => super.visitTerm(term)

  override def visitAtom(atom: Atom): Seq[Atom] = phase match
    case Phase.RemoveVariableAliases => atom match
      case Eq(v1: Var, v2: Var, false) => (v1.typ, v2.typ) match
        case (_, _) if isParam(v1.name) && isParam(v2.name) =>
          super.visitAtom(atom)
        case (Some(TermType(_, Mode.Binding)), Some(TermType(_, Mode.Bound))) =>
          if isParam(v1.name) then
            addParameterAlias(v2.name, v1.name)
            super.visitAtom(atom)
          else
            addAlias(v1.name, v2.name)
            Seq()
        case (Some(TermType(_, Mode.Bound)), Some(TermType(_, Mode.Binding))) =>
          if isParam(v2.name) then
            addParameterAlias(v1.name, v2.name)
            super.visitAtom(atom)
          else
            addAlias(v2.name, v1.name)
            Seq()
        case _ =>
          super.visitAtom(atom)
      case _ => super.visitAtom(atom)
    case Phase.ReplaceVariablesByParameters =>
      super.visitAtom(atom)
    case Phase.SimplifyEqualities => atom match
      case Eq(v1: Var, v2: Var, false) if v1.name == v2.name =>
        Seq()
      case Eq(v1: Var, v2: Var, true) if v1.name == v2.name =>
        throw BodyMustFail(atom)
      case _ => super.visitAtom(atom)
