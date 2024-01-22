package inca.ir.typing

import inca.ir.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.demand.*
import inca.ir.extension.not.*
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

  enum VarMode:
    case Bound
    case Unbound

  type Context = Map[String, (Type, VarMode)]

  enum Boundness:
    case Bound
    case Binding

    def join(that: Boundness): Boundness =
      if (this == Binding || that == Binding)
        Binding
      else
        Bound
  import Boundness.*

  def assertComparable(ty: Type, outside: Type, t: SourceLocation): Unit = (ty, outside) match
    case (TDemand(ty1), TDemand(ty2)) => assertComparable(ty1, ty2, t)
    case (TDemand(_),_) | (_,TDemand(_)) => throw TypeError(s"Cannot compare demanded and undemanded types $ty and $outside")
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

  def checkBound(a: Arg, ctx: Context, expected: Type): Unit = a match
    case WildcardArg() => throw IllegalStateException("Wildcards are not supported")
    case TermArg(t) => checkBound(t, ctx, expected)

  def checkBound(t: Term, ctx: Context, expected: Type): Unit = t match
    case Var(x) => ctx.get(x.name) match
      case Some((ty, VarMode.Bound)) => assertComparable(ty, expected, t)
      case Some((ty, VarMode.Unbound)) => throw TypeError(s"Unbound variable $x not allowed here")
      case None => throw TypeError(s"Undefined variable $x")
    case IntNum(n) => assertComparable(TInt, expected, t)
    case BinOp(t1, t2, "+") =>
      checkBound(t1, ctx, TInt)
      checkBound(t2, ctx, TInt)
      assertComparable(TInt, expected, t)
    case TupleLit(ts) => expected match
      case TTuple(tys) if ts.size == tys.size =>
        ts.zip(tys).foreach { case (tt, tty) => checkBound(tt, ctx, tty) }
      case _ =>
        val tys = ts.map(tt => inferBound(tt, ctx))
        assertComparable(TTuple(tys), expected, t)
    case SetLit(ts) => expected match
      case TSet(tty) =>
        ts.foreach(tt => checkBound(tt, ctx, tty))
      case _ =>
        if (ts.isEmpty) {
          assertComparable(TSet(TNothing), expected, t)
        } else {
          val tys = ts.map(tt => inferBound(tt, ctx))
          val upper = tys.reduce(join)
          assertComparable(TSet(upper), expected, t)
        }
    case SetUnion(t1, t2) => expected match
      case TSet(ty) =>
        checkBound(t1, ctx, expected)
        checkBound(t2, ctx, expected)
      case _ =>
        assertComparable(TSet(TAny), expected, t)
        checkBound(t1, ctx, TSet(TAny))
        checkBound(t2, ctx, TSet(TAny))

  def checkBinding(a: Arg, ctx: Context, expected: Type): (Boundness, Context) = a match
    case WildcardArg() => throw IllegalStateException("Wildcards are not supported")
    case TermArg(t) => checkBinding(t, ctx, expected)

  /**
   * @return (b, ctx) where ctx is the new context and b indicates if t
   */
  def checkBinding(t: Term, ctx: Context, expected: Type): (Boundness, Context) =
    expected match
      case TDemand(ty) =>
        checkBound(t, ctx, ty)
        (Boundness.Bound, ctx)
      case _ => t match
        case Var(RefByName(x)) => ctx.get(x.name) match
          case Some((ty, b)) =>
            assertComparable(ty, expected, t)
            val cl = if (b == VarMode.Bound) Bound else Binding
            (cl, ctx + (x.name -> (ty, VarMode.Bound)))
          case None =>
            (Binding, ctx + (x.name -> (expected, VarMode.Bound)))
        case IntNum(n) =>
          assertComparable(TInt, expected, t)
          (Bound, ctx)
        case BinOp(t1, t2, "+") =>
          checkBound(t1, ctx, TInt)
          checkBound(t2, ctx, TInt)
          assertComparable(TInt, expected, t)
          (Bound, ctx)
        case TupleLit(ts) => expected match
          case TTuple(tys) if ts.size == tys.size =>
            ts.zip(tys).foldLeft((Bound,ctx)) { case ((cl,c), (tt, tty)) =>
              val (cl_, c_) = checkBinding(tt, c, tty)
              (cl join cl_, c_)
            }
          case _ =>
            val (tys, cl, c) = ts.foldLeft((List.empty[Type], Bound, ctx)) { case ((tys, cl, c), tt) =>
              val (tty, cl_, c_) = inferBinding(tt, c)
              (tys :+ tty, cl join cl_, c_)
            }
            assertComparable(TTuple(tys), expected, t)
            (cl, c)
        case SetLit(ts) =>
          checkBound(t, ctx, expected)
          (Bound, ctx)
        case SetUnion(t1, t2) => expected match
          case TSet(ty) =>
            val (cl1, ctx1) = checkBinding(t1, ctx, expected)
            val (cl2, ctx2) = checkBinding(t2, ctx1, expected)
            (cl1 join cl2, ctx2)
          case _ =>
            assertComparable(TSet(TAny), expected, t)
            val (cl1, ctx1) = checkBinding(t1, ctx, TSet(TAny))
            val (cl2, ctx2) = checkBinding(t2, ctx, TSet(TAny))
            (cl1 join cl2, ctx2)


  def inferBound(t: Term, ctx: Context): Type = t match
    case Var(x) => ctx.get(x.name) match
      case Some((ty, VarMode.Bound)) => ty
      case Some((ty, VarMode.Unbound)) => throw TypeError(s"Unbound variable $x not allowed here")
      case None => throw TypeError(s"Undefined variable $x")
    case IntNum(n) => TInt
    case BinOp(t1, t2, "+") =>
      checkBound(t1, ctx, TInt)
      checkBound(t2, ctx, TInt)
      TInt
    case TupleLit(ts) =>
      TTuple(ts.map(tt => inferBound(tt, ctx)))
    case SetLit(ts) =>
      if (ts.isEmpty) {
        TSet(TNothing)
      } else {
        val tys = ts.map(tt => inferBound(tt, ctx))
        val upper = tys.reduce(join)
        TSet(upper)
      }
    case SetUnion(t1, t2) =>
      val TSet(ty1) = inferBoundSet(t1, ctx)
      val TSet(ty2) = inferBoundSet(t2, ctx)
      TSet(join(ty1, ty2))

  def inferBinding(t: Term, ctx: Context): (Type, Boundness, Context) = t match
    case Var(RefByName(x)) => ctx.get(x.name) match
      case Some((ty, b)) =>
        val cl = if (b == VarMode.Bound) Bound else Binding
        (ty, cl, ctx + (x.name -> (ty, VarMode.Bound)))
      case None => throw TypeError(s"Undefined variable $x, cannot infer type")
    case IntNum(n) => (TInt, Bound, ctx)
    case BinOp(t1, t2, "+") =>
      checkBound(t1, ctx, TInt)
      checkBound(t2, ctx, TInt)
      (TInt, Bound, ctx)
    case TupleLit(ts) =>
      val (tys, cl, c) = ts.foldLeft((List.empty[Type],Bound,ctx)) { case ((tys, cl, c), tt) =>
        val (tty, cl_, c_) = inferBinding(tt, ctx)
        (tys :+ tty, cl join cl_, c_)
      }
      (TTuple(tys), cl, c)
    case SetLit(ts) =>
      (inferBound(t, ctx), Bound, ctx)
    case SetUnion(t1, t2) =>
      val (TSet(ty1), cl1, ctx1) = inferBindingSet(t1, ctx)
      val (TSet(ty2), cl2, ctx2) = inferBindingSet(t2, ctx1)
      (TSet(join(ty1, ty2)), cl1 join cl2, ctx2)

  def checkAtomBinding(a: Atom, ctx: Context)(using relations: Relations): Context = a match
    case Call(RefByName(name), args, false) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foldLeft(ctx) { case (c, (tt, tty)) => checkBinding(tt, c, tty)._2 }
    case Call(RefByName(name), args, true) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foreach { case (tt, tty) => checkBound(tt, ctx, tty) }; ctx
    case Eq(t1, t2, false) =>
      Try(inferBound(t1, ctx)) match
        case Success(ty1) => checkBinding(t2, ctx, ty1)._2
        case Failure(err1) => Try(inferBound(t2, ctx)) match
          case Success(ty2) => checkBinding(t1, ctx, ty2)._2
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Eq(t1, t2, true) =>
      Try(inferBound(t1, ctx)) match
        case Success(ty1) => checkBound(t2, ctx, ty1); ctx
        case Failure(err1) => Try(inferBound(t2, ctx)) match
          case Success(ty2) => checkBound(t1, ctx, ty2); ctx
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Not(at) => checkAtomBound(at, ctx)
//    case Demand(ts) => ts.foldLeft(ctx) {case (c, tt) => inferBinding(tt, c)._3 }
    case SetMember(mem, s) =>
      val TSet(tty) = inferBoundSet(s, ctx)
      checkBinding(mem, ctx, tty)._2

  def checkAtomBound(a: Atom, ctx: Context)(using relations: Relations): Context = a match
    case Call(RefByName(name), args, false) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foreach { case (tt, tty) => checkBound(tt, ctx, tty) }; ctx
    case Call(RefByName(name), args, true) => relations.get(name) match
      case None => throw TypeError(s"Relation not found $name")
      case Some(columns) if args.size != columns.size => throw TypeError(s"Wrong number of arguments for $name, expected ${columns.size} but got ${args.size}")
      case Some(columns) => args.zip(columns).foldLeft(ctx) { case (c, (tt, tty)) => checkBinding(tt, c, tty)._2 }
    case Eq(t1, t2, false) =>
      Try(inferBound(t1, ctx)) match
        case Success(ty1) => checkBound(t2, ctx, ty1); ctx
        case Failure(err1) => Try(inferBound(t2, ctx)) match
          case Success(ty2) => checkBound(t1, ctx, ty2); ctx
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Eq(t1, t2, true) =>
      Try(inferBound(t1, ctx)) match
        case Success(ty1) => checkBinding(t2, ctx, ty1)._2
        case Failure(err1) => Try(inferBound(t2, ctx)) match
          case Success(ty2) => checkBinding(t1, ctx, ty2)._2
          case Failure(err2) => throw TypeError(s"Illegal equation $a with two possible errors: " + err1.getMessage + ". " + err2.getMessage)
    case Not(at) => checkAtomBinding(at, ctx)
//    case Demand(ts) => ts.foreach(tt => inferBound(tt, ctx)); ctx
    case SetMember(mem, s) =>
      val TSet(tty) = inferBoundSet(s, ctx)
      checkBound(mem, ctx, tty)
      ctx

  def checkBody(b: Body, ctx: Context)(using Relations): Context =
    b.atoms.foldLeft(ctx) { (c, a) => checkAtomBinding(a, c) }

  def checkParam(p: Param): (Type, VarMode) = p.ty match
    case TDemand(ty) => (ty, VarMode.Bound)
    case ty => (ty, VarMode.Unbound)

  def checkRelation(r: Relation)(using Relations): Unit =
    val ctx = r.params.map(p => p.name.toString -> checkParam(p)).toMap
    r.bodies.foreach { b =>
      val ctxAfter = checkBody(b, ctx)
      r.params.foreach(p => ctxAfter(p.name.toString)._2 match
        case VarMode.Bound => // good
        case VarMode.Unbound => throw TypeError(s"Column ${p.name} of relation ${r.name} is not range restricted in body\n$b")
      )
    }

  def checkModule(m: Module): Unit =
    val rels = m.contents.collect { case r: Relation => r }
    implicit val relations: Relations = rels.map(r => r.name.toString -> r.params.map(_.ty)).toMap
    rels.foreach(checkRelation)


  def inferBoundSet(t: Term, ctx: Context): TSet =
    inferBound(t, ctx) match
      case ty: TSet => ty
      case ty => throw TypeError(s"Expected set type, but $t has type $ty")

  def inferBindingSet(t: Term, ctx: Context): (TSet, Boundness, Context) =
    inferBinding(t, ctx) match
      case (ty: TSet, cl, c) => (ty, cl, c)
      case ty => throw TypeError(s"Expected set type, but $t has type $ty")