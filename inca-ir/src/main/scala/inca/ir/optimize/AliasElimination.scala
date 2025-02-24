package inca.ir.optimize

import inca.ir.typing.Mode
import inca.ir.visitors.IRVisitor
import inca.ir.*

import scala.compiletime.uninitialized

trait AliasElimination extends IRVisitor with Optimizer:
  override def name: String = "AliasElimination"

  enum Phase:
    // Remove simple variable aliases (Note: this ignores equalities that contain a parameter)
    //   e.g. a == 1, b: >< == a: <>, c: >< == b: <>, R(c)  ~>  a == 1, a == a, a == a, R(a)
    case RemoveVariableAliases
    // Replace variables with a parameter, if a corresponding equality exists
    //   e.g. Q(x) :- a: >< == 1, x: >< == a: <>  ~>  Q(x) :- x == 1
    case ReplaceVariablesByParameters
    // Remove equalities where lhs == rhs or lhs != rhs, but lhs is the same var as rhs
    //  e.g. Q(a) :- a == a, R(a)  ~>  Q(a) :- R(a)
    //  e.g. Q(a) :- a != a, R(a)  ~>  Q(a) :- .
    case SimplifyEqualities

  private var phase: Phase = uninitialized

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
    aliases.get(alias) match
      case Some(newTarget) => paramAliases += newTarget -> param
      case _ => paramAliases += alias -> param

  private def lookupAlias(alias: Name): Option[Name] = phase match
    case Phase.RemoveVariableAliases =>
      aliases.get(alias)
    case Phase.ReplaceVariablesByParameters =>
      paramAliases.get(alias)
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
        case FailedBody => Seq()
      }
    }

  override def visitTerm(term: Term): Seq[Term] = term match
    case v: Var if (!isParam(v.name)) => lookupAlias(v.name) match
      case Some(name) => Seq(Var(RefByName(name)))
      case _ => super.visitTerm(v)
    case _ => super.visitTerm(term)

  override def visitAtom(atom: Atom): Seq[Atom] = phase match
    case Phase.RemoveVariableAliases => atom match
      case Eq(v1: Var, v2: Var, false) => (v1.typ.get, v2.typ.get) match
        case (_, _) if isParam(v1.name) && isParam(v2.name) =>
          super.visitAtom(atom)
        case (ty1, ty2) if ty1.mode.isBinding && ty2.mode.isBound =>
          if (isParam(v1.name)) {
            addParameterAlias(v2.name, v1.name)
            super.visitAtom(atom)
          } else {
            logOptimizationStat("variable aliases", 1, _+1)
            addAlias(v1.name, v2.name)
            Seq()
          }
        case (ty1, ty2) if ty1.mode.isBound && ty2.mode.isBinding =>
          if (isParam(v2.name)) {
            addParameterAlias(v1.name, v2.name)
            super.visitAtom(atom)
          } else {
            logOptimizationStat("variable aliases", 1, _+1)
            addAlias(v2.name, v1.name)
            Seq()
          }
        case _ =>
          super.visitAtom(atom)
      case _ => super.visitAtom(atom)
    case Phase.ReplaceVariablesByParameters =>
      super.visitAtom(atom)
    case Phase.SimplifyEqualities => atom match
      case Eq(v1: Var, v2: Var, false) if v1.name == v2.name =>
        logOptimizationStat("identity equations", 1, _+1)
        Seq()
      case Eq(v1: Var, v2: Var, true) if v1.name == v2.name =>
        logOptimizationStat("identity inequations", 1, _+1)
        throw FailedBody
      case _ => super.visitAtom(atom)
