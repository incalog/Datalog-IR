package inca.ir.extension.map.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.map.{MapLit, MapFun, MapComprehension, MapFrom, MapLookUp, MapPlus, MapConcat, MapUnion, MapContains}
import inca.ir.*
import inca.ir.analysis.base.effect.EmptySupplementary
import inca.ir.extension.tuple.analysis.interpreter.TupleOps
import sturdy.data.{MakeJoined, MayJoin, mapJoin}

trait MapOps[V, B]:
  def mapLit(vs: Seq[(V, V)]): V
  def union(ts: Seq[V]): V
  // Check if mem is contained in the set s
  def contains(s: V, mem: V): B
  // Produce an iterable for all values of a set s
  def iter(s: V): Iterable[(V, V)]


trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val mapOps: MapOps[V, B]

  override protected def canDetermineValue(t: Term): Boolean = t match
    case _: MapComprehension => true
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case MapLit(ts) => ???
    case MapFun(params, valTerm) => ???
    case MapComprehension(key, value, atoms) => ???
    case MapFrom(ref) => ???
    case MapLookUp(map, key) => ???
    case MapPlus(map, key, value) => ???
    case MapConcat(t1, t2) => ???
    case MapUnion(t1, t2) => ???
    case _ => super.evalTermOpen(term)

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case MapContains(map, key) => ???
    case _ => super.evalAtomOpen(at)
