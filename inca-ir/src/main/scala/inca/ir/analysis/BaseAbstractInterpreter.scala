package inca.ir.analysis

import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.extension.bool.{BoolFalse, BoolTrue}
import inca.ir.*

trait EqOps[V, B]:
  def equ(v1: V, v2: V): B
  def nequ(v1: V, v2: V): B

trait BooleanOps[V]:
  def boolLit(b: Boolean): V
  def and(v1: V, v2: V): V
  def not(v: V): V
  def or(v1: V, v2: V): V
  def imply(v1: V, v2: V): V = or(not(v1), v2)

trait BooleanBranching[B, R]:
  def boolBranch(v: B, thn: => R, els: => R): R

trait IntegerOps[B, V]:
  def integerLit(i: B): V

  def add(v1: V, v2: V): V
  def sub(v1: V, v2: V): V
  def mul(v1: V, v2: V): V

  def max(v1: V, v2: V): V
  def min(v1: V, v2: V): V
  def absolute(v: V): V

  def div(v1: V, v2: V): V
  def remainder(v1: V, v2: V): V

trait FloatOps[B, V]:
  def floatingLit(f: B): V
  def randomFloat(): V

  def add(v1: V, v2: V): V
  def sub(v1: V, v2: V): V
  def mul(v1: V, v2: V): V
  def div(v1: V, v2: V): V
  def min(v1: V, v2: V): V
  def max(v1: V, v2: V): V

  def absolute(v: V): V

trait OrderingOps[V, B]:
  def lt(v1: V, v2: V): B
  def le(v1: V, v2: V): B

  def ge(v1: V, v2: V): B = le(v2, v1)
  def gt(v1: V, v2: V): B = lt(v2, v1)


trait BaseAbstractInterpreter[V, B]:

  case object TermKey extends AnalysisKey:
    override val key: String = "Term"
    override type Result = TermResult
  case class TermResult(value: V, pure: B) extends AnalysisResult:
    val result: TermResult = this
    override val akey: TermKey.type = TermKey
    def &&(otherPure: B): TermResult =
      TermResult(value, boolOps.and(pure, otherPure))
    override def toString: String = s"$value{$pure}"

  case object AtomKey extends AnalysisKey:
    override val key: String = "Atom"
    override type Result = AtomResult
  case class AtomResult(value: B, pure: B) extends AnalysisResult:
    val result: AtomResult = this
    override val akey: AtomKey.type = AtomKey
    def &&(other: AtomResult): AtomResult =
      AtomResult(boolOps.and(value, other.value), boolOps.and(pure, other.pure))
    def &&(otherPure: B): AtomResult =
      AtomResult(value, boolOps.and(pure, otherPure))

  def trueBool: B = boolOps.boolLit(true)
  def falseBool: B = boolOps.boolLit(false)

  val boolOps: BooleanOps[B]
  val eqOps: EqOps[V, B]

  var env: Map[String, V] = Map()
  def inScope[A](f: => A): A =
    val before = env
    try f
    finally env = before

  def top: V
  def topBool: B

  def evalModule(m: Module): Unit =
    m.relations.values.foreach(evalRelation)

  def evalRelation(r: Relation): Unit =
    r.bodies.foreach { b =>
      inScope {
        evalBody(b)
      }
    }

  def evalBody(b: Body): B =
    var rest = b.atoms
    var bodySuccess = trueBool
    while (rest.nonEmpty) {
      val aa = evalAtom(rest.head)
      // if (a == false) return false
      bodySuccess = boolOps.and(bodySuccess, aa.value)
      rest = rest.tail
    }
    bodySuccess

  final def evalAtom(at: Atom): AtomResult =
    val b = evalAtomExtend(at)
    at.storeAnalysisResult(b)
    b

  def evalAtomExtend(at: Atom): AtomResult = at match
    case Eq(lhs, rhs) => evalEquals(lhs, rhs)
    case Neq(lhs, rhs) => evalNotEquals(lhs, rhs)
    case Call(name, args) => evalCall(name, args)
    case NegCall(name, args) => evalCall(name, args)
    case ExtensionalCall(name, args) => evalCall(name, args)
    case NegExtensionalCall(name, args) => evalCall(name, args)

  final def evalCall(ref: Ref[Relation], args: Seq[Term]): AtomResult =
    val vs = args.map { a =>
      if (a.mode.isBinding) {
        val t = top
        val AtomResult(_, p) = assign(a, t)
        TermResult(t, p)
      } else if (a.mode.isCollapse) {
        // skip collapsed argument
        TermResult(top, trueBool)
      } else {
        evalTerm(a)
      }
    }
    AtomResult(topBool, vs.foldLeft(trueBool)((p, v) => boolOps.and(p, v.pure)))

  final def evalNotEquals(lhs: Term, rhs: Term): AtomResult =
    val TermResult(vl, pl) = evalTerm(lhs)
    val TermResult(vr, pr) = evalTerm(rhs)
    AtomResult(eqOps.nequ(vl, vr), boolOps.and(pl, pr))

  final def evalEquals(lhs: Term, rhs: Term): AtomResult =
    if (rhs.mode.isBinding) {
      val v = evalTerm(lhs)
      assign(rhs, v.value) && v.pure
    }
    else if (lhs.mode.isBinding) {
      val v = evalTerm(rhs)
      assign(lhs, v.value) && v.pure
    } else {
      val TermResult(vl, pl) = evalTerm(lhs)
      val TermResult(vr, pr) = evalTerm(rhs)
      AtomResult(eqOps.equ(vl, vr), boolOps.and(pl, pr))
    }

  final def assign(assignee: Term, v: V): AtomResult = assignee match
    case Var(x) =>
      if (assignee.mode.isBinding) {
        env += x.name -> v
        assignee.storeAnalysisResult(TermResult(v, trueBool))
        AtomResult(trueBool, trueBool)
      } else {
        // FIXME: Is this correct ? This can happen e.g. when a destruct fails, since then
        //  a variable might be unbound and not in the environment.
        val boundV = env.getOrElse(x.name, top)
        assignee.storeAnalysisResult(TermResult(boundV, trueBool))
        AtomResult(eqOps.equ(boundV, v), trueBool)
      }
    case Cast(t, ty) =>
      val r = assign(t, v)
      assignee.storeAnalysisResult(r)
      r

  final def evalTerm(t: Term): TermResult =
    val r = evalTermExtend(t)
    t.storeAnalysisResult(r)
    r

  protected def evalTermExtend(t: Term): TermResult = t match
    case Var(x) =>
      // FIXME: Is this correct ? See above
      TermResult(env.getOrElse(x, top), trueBool)
    case Cast(t, ty) => evalTerm(t)

//trait BoolOps[V]:
//  def and(v1: V, v2: V): V
//  def or(v1: V, v2: V): V
//
//
//trait BoolAbstractInterpreter[V] extends BaseAbstractInterpreter[V]:
//  import inca.ir.extension.bool.*
//
//  val boolOps: BoolOps[V]
//
//  override def evalTerm(t: Term): V = t match
//    case BoolAnd(t1, t2) => boolOps.and(evalTerm(t1), evalTerm(t2))
//    case BoolOr(t1, t2) => boolOps.or(evalTerm(t1), evalTerm(t2))
//    case _ => super.evalTerm(t)
//


//
//  override def evalTerm(t: Term): V = t match
//    case _ => super.evalTerm(t)
//
//

//
//enum AbsBool:
//  case True
//  case False
//  case Top
//
//enum Value:
//  case Bool(b: AbsBool)
//  case Int(low: Int, high: Int)


//class AbstractInterpreter extends BaseAbstractInterpreter[Value]
//  with BoolAbstractInterpreter[Value]:
//
//  val boolOps = new BoolOps[Value]:
//    override def and(v1: Value, v2: Value): Value = (v1, v2) match
//      case (Value.Bool(AbsBool.True), Value.Bool(b2)) => v2
//      case (Value.Bool(AbsBool.False), Value.Bool(b2)) => v1
//      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.True)) => v1
//      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.False)) => v2
//
//    override def or(v1: Value, v2: Value): Value = (v1, v2) match
//      case (Value.Bool(AbsBool.True), Value.Bool(b2)) => v1
//      case (Value.Bool(AbsBool.False), Value.Bool(b2)) => v2
//      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.True)) => v2
//      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.False)) => v1
//
//val interp = new AbstractInterpreter
//
//
//def boolOptimize(t: Term): Term = t match
////  case BoolAnd(BoolTrue(), t2) => t2
////  case BoolAnd(BoolFalse(), t2) => BoolFalse()
//  case BoolAnd(t1, t2) =>
//    if (t1.absval == Value.Bool(AbsBool.True))
//      t2
//  case BoolNot(BoolNot(t)) => t