//package inca.ir
//
//import inca.ir.extension.bool.{BoolFalse, BoolTrue}
//import inca.ir.typing.Mode
//
//trait BaseAbstractInterpreter[V]:
//
//  var env: Map[String, V] = Map()
//
//  def evalAtom(at: Atom, m: Mode): Unit = ???
//
//  def evalTerm(t: Term): V = t match
//    case Var(x) => env(x)
//    case Cast(t, ty) => evalTerm(t)
//
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
//trait ArithmeticAbstractInterpreter[V] extends BaseAbstractInterpreter[V]:
//
//  val intOps: IntOps[V]
//  val doubleOps: DoubleOps[V]
//
//  override def evalTerm(t: Term): V = t match
//    case _ => super.evalTerm(t)
//
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
//
//
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