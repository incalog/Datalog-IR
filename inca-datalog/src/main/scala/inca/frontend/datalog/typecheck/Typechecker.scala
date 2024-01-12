package inca.frontend.datalog.typecheck

import inca.frontend.datalog.syntax.*
import inca.ir.Name
import inca.ir.typing.{TypeIO, Typeable}
import inca.ir.util.SourceLocation

class Typechecker extends TypeIO {

  var ctx: Map[Name, Type] = Map()
  var rels: Map[Name, (Seq[Type], IRelation)] = Map()

  val arithOps = Set("+","-","*","/")

  def checkModule(m: Module): Unit =
    rels = m.relations.map(r => r.name -> (r.params, r)).toMap
    m.relations.foreach(checkModuleEntry)

  def checkModuleEntry(me: IRelation): Unit = me match
    case r: EdbRelation => // nothing
    case r: Relation => checkRelation(r)

  def checkRelation(r: Relation): Unit =
    r.rules.foreach(checkRule(_, r.params))

  def checkRule(r: Rule, paramTypes: Seq[Type]): Unit =
    ctx = r.head.zip(paramTypes).flatMap {
      case (c@Param.Constant(l), ty) => assertComparable(inferLiteral(l), ty, c); None
      case (Param.Named(n), ty) => Some(n -> ty)
      case (Param.Aggregated(n, op), ty) => Some(n -> ty)
    }.toMap
    r.body.foreach(checkAtom)

  def checkAtom(a: Atom): Unit = a match
    case Atom.Call(ref, args, not) => rels.get(ref.name) match
      case None =>
        error(s"Unknown relation ${ref.name}", a)
        args.foreach(inferTermOpt)
      case Some((params, rel)) =>
        ref.resolved(rel)
        if (args.size != params.size)
          error(s"Wrong number of arguments, expected ${params.size} but got ${args.size}", a)
        args.zip(params).foreach(checkTerm)
    case Atom.Compare(lhs, "==", rhs) => inferTermOpt(lhs) match
      case Some(ty) => checkTerm(rhs, ty)
      case None => inferTermOpt(rhs) match
        case Some(ty) => checkTerm(lhs, ty)
        case None => error(s"Cannot infer type for either side of equality constraint", a)
    case Atom.Compare(lhs, "!=", rhs) =>
      assertComparable(inferTerm(lhs), inferTerm(rhs), a)
    case Atom.Compare(lhs, "<", rhs) =>
      assertComparable(inferTerm(lhs), inferTerm(rhs), a)
    case Atom.Compare(lhs, "<=", rhs) =>
      assertComparable(inferTerm(lhs), inferTerm(rhs), a)
    case Atom.Compare(lhs, ">", rhs) =>
      assertComparable(inferTerm(lhs), inferTerm(rhs), a)
    case Atom.Compare(lhs, ">=", rhs) =>
      assertComparable(inferTerm(lhs), inferTerm(rhs), a)
    case _ => error(s"Unknown atom",a )

  def checkTerm(t: Term, ty: Type): Unit = t match
    case Term.Var(name) => ctx.get(name) match
      case None =>
        ctx += name -> ty
        assignType(t)(ty)
      case Some(ty2) => assertComparable(ty, ty2, t)
    case _ =>
      assertComparable(ty, inferTerm(t), t)

  def assertComparable(ty1: Type, ty2: Type, s: SourceLocation): Unit =
    if (ty1 != ty2)
      error(s"Incompatible types $ty1 and $ty2 in $s", s)

  def inferTerm(t: Term): Type =
    inferTermOpt(t).getOrElse { error(s"Cannot infer type of $t", t); Type.Int() }

  def inferTermOpt(t: Term): Option[Type] = assignTypeOpt(t) { t match
    case Term.Var(name) => ctx.get(name)
    case Term.Constant(lit) => Some(inferLiteral(lit))
    case Term.BinOp(lhs, op, rhs) => (inferTerm(lhs), inferTerm(rhs)) match
      case (Type.Int(), Type.Int()) =>
        if (!arithOps.contains(op)) {
          error(s"Unknown integer operator $op", t)
          None
        } else {
          Some(Type.Int())
        }
      case (Type.Double(), Type.Double()) =>
        if (!arithOps.contains(op)) {
          error(s"Unknown integer operator $op", t)
          None
        } else {
          Some(Type.Double())
        }
      case (Type.String(), Type.String()) =>
        if (op != "==") {
          error(s"Unknown string operator $op", t)
          None
        } else {
          Some(Type.String())
        }
      case (ty1, ty2) =>
        error(s"Incompatible types $ty1 and $ty2 for operator $op", t)
        None
  }

  def inferLiteral(l: Literal): Type = assignType(l) { l match
    case Literal.Int(i) => Type.Int()
    case Literal.Double(d) => Type.Double()
    case Literal.String(s) => Type.String()
  }

  def assignType(t: Typeable[Type])(f: => Type): Type =
    val ty = f
    t.typed(ty)
    ty

  def assignTypeOpt(t: Typeable[Type])(f: => Option[Type]): Option[Type] =
    val ty = f
    ty.foreach(t.typed(_))
    ty
}
