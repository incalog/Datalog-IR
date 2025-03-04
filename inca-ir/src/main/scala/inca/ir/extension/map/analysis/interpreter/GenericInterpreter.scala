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
  def concat(m1: V, m2: V): V
  def union(ts: Seq[V]): V
  def plus(m: V, k: V, v: V): V
  def lookup(m: V, k: V): Seq[V]
  // Check if mem is contained in the set m
  def contains(s: V, mem: V): B
  // Produce an iterable for all values of a set m
  def iter(s: V): Iterable[(V, V)]


trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val mapOps: MapOps[V, B]

  private def naryTupleOp(rs: Seq[(SupColumn, SupColumn)])(f: Seq[(V, V)] => V): SupColumn =
    val resName = gensym.fresh("result")
    updateSupplementaryUnchecked { sup =>
      val idx = rs.map { (col1, col2) =>
        val idx1 = relationOps.columnIndex(sup, col1)
        val idx2 = relationOps.columnIndex(sup, col2)
        (idx1, idx2)
      }
      relationOps.map(sup, resName) { row => f(idx.map((idx1, idx2) => (row(idx1), row(idx2)))) }
    }
    resName

  private def evalTermTuple(tup: (ir.Term, ir.Term))(using Fixed): (SupColumn, SupColumn) =
    (evalTerm(tup._1), evalTerm(tup._2))

  override protected def canDetermineValue(t: Term): Boolean = t match
    case _: MapComprehension => true
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case MapLit(ts) => naryTupleOp(ts.map(evalTermTuple))(mapOps.mapLit)
    case MapFun(params, valTerm) => ???
    case MapComprehension(key, value, atoms) => ???
    case MapFrom(ref) => ???
    case MapLookUp(map, key) =>
      val resName = gensym.fresh("result")
      updateSupplementaryUnchecked { sup =>
        val mapIx = relationOps.columnIndex(sup, evalTerm(map))
        val keyIx = relationOps.columnIndex(sup, evalTerm(key))
        relationOps.flatMap(sup) { row => 
          val vs = mapOps.lookup(row(mapIx), row(keyIx))
          mapJoin(vs, { value =>
            relationOps.map(sup, resName) { _ => value }
          })
        }
      }
      resName
    case MapPlus(map, key, value) =>
      ternaryOp(evalTerm(map), evalTerm(key), evalTerm(value))(mapOps.plus)
    case MapConcat(t1, t2) =>
      binaryOp(evalTerm(t1), evalTerm(t2))(mapOps.concat)
    case MapUnion(t1, t2) =>
      naryOp(Seq(t1, t2).map(evalTerm))(mapOps.union)
    case _ => super.evalTermOpen(term)

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case MapContains(map, key) => ???
    case _ => super.evalAtomOpen(at)
