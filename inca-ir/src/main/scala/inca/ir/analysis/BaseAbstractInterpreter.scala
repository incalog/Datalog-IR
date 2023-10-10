//package inca.ir
//
//import inca.ir.extension.bool.{BoolFalse, BoolTrue}
//import inca.ir.typing.Mode
//
//trait EqOps[V, B]:
//  def equ(v1: V, v2: V): B
//
//trait BooleanBranching[B, R]:
//  def boolBranch(v: B, thn: => R, els: => R): R
//
//trait BaseAbstractInterpreter[V, B]:
//
//  val eqOps: EqOps[V, B]
//  val branchOps: BooleanBranching[B, Unit]
//
//  var env: Map[String, V] = Map()
//  var state: Map[String, T] = Map()
//
//  def evalAtom(at: Atom): Unit = at match
//    case Eq(lhs, rhs) => evalEquals(lhs, rhs)
//    case Neq(lhs, rhs) => evalEquals(lhs, rhs)
//    case Call(name, args) =>
//
//  final def evalEquals(lhs: Term, rhs: Term): Unit =
//    if (rhs.typ.get.mode.isBinding)
//      assign(rhs, evalTerm(lhs))
//    else
//      assign(lhs, evalTerm(rhs))
//
//  def assign(assignee: Term, v: V): Unit = assignee match
//    case Var(x) =>
//      if (assignee.typ.get.mode.isBinding)
//        env += x.name -> v
//      else
//        assertEquals(env(x.name), v)
//    case Cast(t, ty) => assign(t, v)
//
//  final def assertEquals(v1: V, v2: V): Unit =
//    val isEqual = eqOps.equ(v1, v2)
//    branchOps.boolBranch(isEqual, (), ??? /* throws body failed */)
//
//  def evalTerm(t: Term): V = t match
//    case Var(x) => env(x)
//    case Cast(t, ty) => evalTerm(t)
//
////trait BoolOps[V]:
////  def and(v1: V, v2: V): V
////  def or(v1: V, v2: V): V
////
////
////trait BoolAbstractInterpreter[V] extends BaseAbstractInterpreter[V]:
////  import inca.ir.extension.bool.*
////
////  val boolOps: BoolOps[V]
////
////  override def evalTerm(t: Term): V = t match
////    case BoolAnd(t1, t2) => boolOps.and(evalTerm(t1), evalTerm(t2))
////    case BoolOr(t1, t2) => boolOps.or(evalTerm(t1), evalTerm(t2))
////    case _ => super.evalTerm(t)
////
////trait ArithmeticAbstractInterpreter[V] extends BaseAbstractInterpreter[V]:
////
////  val intOps: IntOps[V]
////  val doubleOps: DoubleOps[V]
////
////  override def evalTerm(t: Term): V = t match
////    case _ => super.evalTerm(t)
////
////
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
//
//
////class AbstractInterpreter extends BaseAbstractInterpreter[Value]
////  with BoolAbstractInterpreter[Value]:
////
////  val boolOps = new BoolOps[Value]:
////    override def and(v1: Value, v2: Value): Value = (v1, v2) match
////      case (Value.Bool(AbsBool.True), Value.Bool(b2)) => v2
////      case (Value.Bool(AbsBool.False), Value.Bool(b2)) => v1
////      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.True)) => v1
////      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.False)) => v2
////
////    override def or(v1: Value, v2: Value): Value = (v1, v2) match
////      case (Value.Bool(AbsBool.True), Value.Bool(b2)) => v1
////      case (Value.Bool(AbsBool.False), Value.Bool(b2)) => v2
////      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.True)) => v2
////      case (Value.Bool(AbsBool.Top), Value.Bool(AbsBool.False)) => v1
////
////val interp = new AbstractInterpreter
////
////
////def boolOptimize(t: Term): Term = t match
//////  case BoolAnd(BoolTrue(), t2) => t2
//////  case BoolAnd(BoolFalse(), t2) => BoolFalse()
////  case BoolAnd(t1, t2) =>
////    if (t1.absval == Value.Bool(AbsBool.True))
////      t2
////  case BoolNot(BoolNot(t)) => t