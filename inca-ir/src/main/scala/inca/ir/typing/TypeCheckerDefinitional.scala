package inca.ir.typing

import inca.ir.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.demand.*
import inca.ir.extension.not.*
import inca.ir.extension.primitiveScala.*
import inca.ir.extension.set.{TSet, *}
import inca.ir.extension.tuple.*
import inca.ir.util.SourceLocation

import scala.util.{Failure, Success, Try}

//noinspection DuplicatedCode,ScalaWeakerAccess
object TypeCheckerDefinitional:

  /*
     1. checking or inferring
     2. closed or closing
   */

  type Relations = Map[String, Seq[Type]]

  case class TypeError(msg: String) extends Exception(msg)

  enum Boundedness:
    case Bound
    case Unbound
  import Boundedness.*
  type Context = Map[String, (Type, Boundedness)]

  def assertComparable(ty: Type, outside: Type, t: SourceLocation): Unit = (ty, outside) match
    case (TAny,_) | (_, TAny) => // fine
    case  (_, TNothing) => throw TypeError(s"Expected type $outside, which cannot be inhabited by $t")
    case (TNothing, _) => throw TypeError(s"Expected type $outside, but $t has type $ty")
    case (TSet(tty1), TSet(tty2)) => assertComparable(tty1, tty2, t)
    case (TTuple(ttys1), TTuple(ttys2)) if ttys1.size == ttys2.size =>
      ttys1.zip(ttys2).foreach(assertComparable(_, _, t))
    case _ =>
      if (ty == outside) {
        // fine
      } else {
        throw TypeError(s"Expected type $outside, but $t has type $ty")
      }

  def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match
    case (TAny,_) | (_, TAny) => TAny
    case (TNothing, _) => ty2
    case (_, TNothing) => ty1
    case (TSet(tty1), TSet(tty2)) => TSet(join(tty1, tty2))
    case (TTuple(ttys1), TTuple(ttys2)) if ttys1.size == ttys2.size =>
      TTuple(ttys1.zip(ttys2).map(join))
    case _ => if (ty1 == ty2) ty1 else TAny

  def checkClosed(t: Term, ctx: Context, expected: Type): Unit = t match
    case Var(x) => ctx.get(x.name) match
      case Some((ty, Bound)) => assertComparable(ty, expected, t)
      case Some((ty, Unbound)) => throw TypeError(s"Unbound variable $x not allowed here")
      case None => throw TypeError(s"Undefined variable $x")
    case IntNum(n) => assertComparable(TInt, expected, t)
    case Add(t1, t2) =>
      checkClosed(t1, ctx, TInt)
      checkClosed(t2, ctx, TInt)
      assertComparable(TInt, expected, t)
    case Tuple(ts) => expected match
      case TTuple(tys) if ts.size == tys.size =>
        ts.zip(tys).foreach { case (tt, tty) => checkClosed(tt, ctx, tty) }
      case _ =>
        val tys = ts.map(tt => inferClosed(tt, ctx))
        assertComparable(TTuple(tys), expected, t)
    case Set(ts) => expected match
      case TSet(tty) =>
        ts.foreach(tt => checkClosed(tt, ctx, tty))
      case _ =>
        if (ts.isEmpty) {
          assertComparable(TSet(TNothing), expected, t)
        } else {
          val tys = ts.map(tt => inferClosed(tt, ctx))
          val upper = tys.reduce(join)
          assertComparable(TSet(upper), expected, t)
        }
    case SetUnion(t1, t2) => expected match
      case TSet(ty) =>
        checkClosed(t1, ctx, expected)
        checkClosed(t2, ctx, expected)
      case _ =>
        assertComparable(TSet(TAny), expected, t)
        checkClosed(t1, ctx, TSet(TAny))
        checkClosed(t2, ctx, TSet(TAny))

  def checkClosing(t: Term, ctx: Context, expected: Type): Context = t match
    case Var(x) => ctx.get(x.name) match
      case Some((ty, b)) =>
        assertComparable(ty, expected, t)
        ctx + (x.name -> (ty, Bound))
      case None =>
        ctx + (x.name -> (expected, Bound))
    case IntNum(n) =>
      assertComparable(TInt, expected, t)
      ctx
    case Add(t1, t2) =>
      checkClosed(t1, ctx, TInt)
      checkClosed(t2, ctx, TInt)
      assertComparable(TInt, expected, t)
      ctx
    case Tuple(ts) => expected match
      case TTuple(tys) if ts.size == tys.size =>
        ts.zip(tys).foldLeft(ctx) { case (c, (tt, tty)) => checkClosing(tt, c, tty) }
      case _ =>
        val (tys, c) = ts.foldLeft((List.empty[Type], ctx)) { case ((tys, c), tt) =>
          val (tty, c_) = inferClosing(tt, c)
          (tys :+ tty, c_)
        }
        assertComparable(TTuple(tys), expected, t)
        c
    case Set(ts) => checkClosed(t, ctx, expected); ctx
    case SetUnion(t1, t2) => checkClosed(t, ctx, expected); ctx

  def inferClosed(t: Term, ctx: Context): Type = t match
    case Var(x) => ctx.get(x.name) match
      case Some((ty, Bound)) => ty
      case Some((ty, Unbound)) => throw TypeError(s"Unbound variable $x not allowed here")
      case None => throw TypeError(s"Undefined variable $x")
    case IntNum(n) => TInt
    case Add(t1, t2) =>
      checkClosed(t1, ctx, TInt)
      checkClosed(t2, ctx, TInt)
      TInt
    case Tuple(ts) =>
      TTuple(ts.map(tt => inferClosed(tt, ctx)))
    case Set(ts) =>
      if (ts.isEmpty) {
        TSet(TNothing)
      } else {
        val tys = ts.map(tt => inferClosed(tt, ctx))
        val upper = tys.reduce(join)
        TSet(upper)
      }
    case SetUnion(t1, t2) =>
      val TSet(ty1) = inferClosedSet(t1, ctx)
      val TSet(ty2) = inferClosedSet(t2, ctx)
      TSet(join(ty1, ty2))

  def inferClosing(t: Term, ctx: Context): (Type, Context) = t match
    case Var(x) => ctx.get(x.name) match
      case Some((ty, b)) =>
        (ty, ctx + (x.name -> (ty, Bound)))
      case None => throw TypeError(s"Undefined variable $x, cannot infer type")
    case IntNum(n) => (TInt, ctx)
    case Add(t1, t2) =>
      checkClosed(t1, ctx, TInt)
      checkClosed(t2, ctx, TInt)
      (TInt, ctx)
    case Tuple(ts) =>
      val (tys, c) = ts.foldLeft((List.empty[Type],ctx)) { case ((tys, c), tt) =>
        val (tty, c_) = inferClosing(tt, ctx)
        (tys :+ tty, c_)
      }
      (TTuple(tys), c)
    case Set(ts) =>
      (inferClosed(t, ctx), ctx)
    case SetUnion(t1, t2) =>
      (inferClosed(t, ctx), ctx)

  def checkAtomClosing(a: Atom, ctx: Context)(using relations: Relations): Context = a match
    case Call(name, args) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foldLeft(ctx) { case (c, (tt, tty)) => checkClosing(tt, c, tty) }
    case NegCall(name, args) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foreach { case (tt, tty) => checkClosed(tt, ctx, tty) }; ctx
    case Eq(t1, t2) =>
      Try(inferClosed(t1, ctx)) match
        case Success(ty1) => checkClosing(t2, ctx, ty1)
        case Failure(err1) => Try(inferClosed(t2, ctx)) match
          case Success(ty2) => checkClosing(t1, ctx, ty2)
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Neq(t1, t2) =>
      Try(inferClosed(t1, ctx)) match
        case Success(ty1) => checkClosed(t2, ctx, ty1); ctx
        case Failure(err1) => Try(inferClosed(t2, ctx)) match
          case Success(ty2) => checkClosed(t1, ctx, ty2); ctx
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Not(at) => checkAtomClosed(at, ctx)
    case Demand(ts) => ts.foldLeft(ctx) {case (c, tt) => inferClosing(tt, c)._2 }
    case SetMember(mem, s) =>
      val TSet(tty) = inferClosedSet(s, ctx)
      checkClosing(mem, ctx, tty)

  def checkAtomClosed(a: Atom, ctx: Context)(using relations: Relations): Context = a match
    case Call(name, args) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foreach { case (tt, tty) => checkClosed(tt, ctx, tty) }; ctx
    case NegCall(name, args) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foldLeft(ctx) { case (c, (tt, tty)) => checkClosing(tt, c, tty) }
    case Eq(t1, t2) =>
      Try(inferClosed(t1, ctx)) match
        case Success(ty1) => checkClosed(t2, ctx, ty1); ctx
        case Failure(err1) => Try(inferClosed(t2, ctx)) match
          case Success(ty2) => checkClosed(t1, ctx, ty2); ctx
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Neq(t1, t2) =>
      Try(inferClosed(t1, ctx)) match
        case Success(ty1) => checkClosing(t2, ctx, ty1)
        case Failure(err1) => Try(inferClosed(t2, ctx)) match
          case Success(ty2) => checkClosing(t1, ctx, ty2)
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Not(at) => checkAtomClosing(at, ctx)
    case Demand(ts) => ts.foreach(tt => inferClosed(tt, ctx)); ctx
    case SetMember(mem, s) =>
      val TSet(tty) = inferClosedSet(s, ctx)
      checkClosed(mem, ctx, tty)
      ctx

  def checkBody(b: Body, ctx: Context)(using Relations): Context =
    b.atoms.foldLeft(ctx) { (c, a) => checkAtomClosing(a, c) }

  def checkRelation(r: Relation)(using Relations): Unit =
    val ctx = r.params.map(p => p.name.toString -> (p.ty, Unbound)).toMap
    r.bodies.foreach { b =>
      val ctxAfter = checkBody(b, ctx)
      r.params.foreach(p => ctxAfter(p.name.toString)._2 match
        case Bound => // good
        case Unbound => throw TypeError(s"Column ${p.name} of relation ${r.name} is not range restricted in body\n$b")
      )
    }

  def checkModule(m: Module): Unit =
    val rels = m.contents.collect { case r: Relation => r }
    implicit val relations: Relations = rels.map(r => r.name.toString -> r.params.map(_.ty)).toMap
    rels.foreach(checkRelation)


  def inferClosedSet(t: Term, ctx: Context): TSet =
    inferClosed(t, ctx) match
      case ty: TSet => ty
      case ty => throw TypeError(s"Expected set type, but $t has type $ty")