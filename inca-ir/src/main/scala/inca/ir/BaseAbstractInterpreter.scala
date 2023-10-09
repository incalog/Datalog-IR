//package inca.ir
//
//
//
//trait BaseAbstractInterpreter[V]:
//
//  var env: Map[String, V] = Map()
//
//  def evalAtom(at: Atom): Unit = ???
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
//  val boolOps: BoolOps[V]
//
//  override def evalTerm(t: Term): V = t match
//    case BoolAnd(t1, t2) => boolOps.and(evalTerm(t1), evalTerm(t2))
//    case BoolOr(t1, t2) => boolOps.or(evalTerm(t1), evalTerm(t2))
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
