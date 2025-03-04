package inca.ir.extension.map.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapFun, MapLit, MapLookUp, MapPlus, MapUnion}
import inca.ir.*
import inca.ir.analysis.base.effect.EmptySupplementary
import inca.ir.extension.demand.TDemand
import inca.ir.extension.set.analysis.interpreter.SetOps
import inca.ir.extension.tuple.analysis.interpreter.TupleOps
import sturdy.data.{MakeJoined, MayJoin, mapJoin}

trait MapOps[V, B]:
  def mapLit(vs: Seq[(V, V)]): V
  def concat(m1: V, m2: V): V
  def union(ts: Seq[V]): V
  def plus(m: V, k: V, v: V): V
  def lookup(m: V, k: V): Seq[V]
  // Check if key is contained in the map m
  def contains(m: V, key: V): B
  // Produce an iterable for all keys in map m
  def keyIter(m: V): Iterable[V]


trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  // We need both of these for MapFrom
  val tupleOps: TupleOps[V]
  val setOps: SetOps[V, B]

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
    case _: MapFun => true
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case MapLit(ts) => naryTupleOp(ts.map(evalTermTuple))(mapOps.mapLit)
    case MapConcat(t1, t2) => binaryOp(evalTerm(t1), evalTerm(t2))(mapOps.concat)
    case MapPlus(map, key, value) => ternaryOp(evalTerm(map), evalTerm(key), evalTerm(value))(mapOps.plus)
    case MapUnion(t1, t2) => naryOp(Seq(t1, t2).map(evalTerm))(mapOps.union)
    case MapFun(params, valTerm) => ???
    case MapComprehension(key, value, atoms) =>
      val resultColumn = gensym.fresh("result")
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        except.tryCatch {
          evalAtoms(atoms)
          val keyCol = evalTerm(key)
          val valCol = evalTerm(value)
          val newSup = supplementaryTable.getTable
          relationOps.groupBy(newSup, Seq(keyCol, valCol), columnsBefore)(columnsBefore :+ resultColumn, {
            (groupedVals, elemVals: Seq[Seq[V]]) =>
              groupedVals :+ mapOps.mapLit(elemVals.map(kv => kv.head -> kv.last))
          })
        } /* catch */ { exec =>
          // FIXME: To be in accordance with the lowering we need to differentiate empty maps based on the type
          //  e.g Map[K, V]() != Map[K1, V1]() if (K1 != K) || (V != V1)
          relationOps.map(sup, resultColumn) { _ => mapOps.mapLit(Seq()) }
        }(using mayJoinRV)
      }
      resultColumn
    case MapFrom(ref) =>
      val r = ref.target.getOrElse(throw new IllegalStateException(s"Unknown relation ${ref.name}"))
      // 1. Everything that is demanded is a key, the rest is a value
      val (demanded, nondemanded) = r.params.partition(_.ty.isInstanceOf[TDemand])

      val resultColumn = gensym.fresh("result")
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        except.tryCatch {
          // 2. Evaluate the relation we want to convert to a map
          val accCols = r.params.map(_ => gensym.fresh("arg"))
          val args = accCols.map(c => ir.TermArg(Var(c)))
          evalCall(r, r.params, args, false)
          val newSup = supplementaryTable.getTable

          // Confusing behaviour, but in accordance to the lowering.
          relationOps.groupBy(newSup, accCols, columnsBefore)(columnsBefore :+ resultColumn, {
            case (groupedVals, accVals) if demanded.isEmpty =>
              // 3. Create a set if we don't have demanded parameters aka keys
              if (accCols.size == 1)
                // Don't create unary tuples
                groupedVals :+ setOps.setLit(accVals.flatten)
              else
                val tups = accVals.map(tupleOps.tupleLit)
                groupedVals :+ setOps.setLit(tups)
            case (groupedVals, accVals: Seq[Seq[V]]) =>
              // 4. Create a map only if we have demanded parameters
              val kvs = accVals.map { row =>
                val (namedInputVals, namedOutputVals) = accCols.zip(row).partition((c, _) => demanded.contains(c))
                val inputVals = namedInputVals.map(_._2)
                val outputVals = namedOutputVals.map(_._2)
                // Don't create unary tuples
                (demanded.size, nondemanded.size) match
                  case (1, 1) => inputVals.head -> outputVals.head
                  case (1, _) => inputVals.head -> tupleOps.tupleLit(outputVals)
                  case (_, 1) => tupleOps.tupleLit(inputVals) -> outputVals.head
              }
              val map = mapOps.mapLit(kvs)
              groupedVals :+ map
          })
        } /* catch */ { exec =>
          // FIXME: To be in accordance with the lowering we need to differentiate empty maps based on the type
          //  e.g Map[K, V]() != Map[K1, V1]() if (K1 != K) || (V != V1)
          relationOps.map(sup, resultColumn) { _ => mapOps.mapLit(Seq()) }
        }(using mayJoinRV)
      }
      resultColumn
    case MapLookUp(map, key) =>
      val resName = gensym.fresh("result")
      updateSupplementaryUnchecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        val mapIx = relationOps.columnIndex(sup, evalTerm(map))
        val keyIx = relationOps.columnIndex(sup, evalTerm(key))
        relationOps.flatMap(sup) { row =>
          val vs = mapOps.lookup(row(mapIx), row(keyIx))
          relationOps.make(columnsBefore :+ resName, vs.map(v => row :+ v))
        }
      }
      resName
    case _ => super.evalTermOpen(term)

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case MapContains(map, key) if canDetermineValue(key) => // containment check
      val keyCol = evalTerm(key)
      val mapCol = evalTerm(map)
      updateSupplementaryChecked { sup =>
        val keyIdx = relationOps.columnIndex(sup, keyCol)
        val mapIdx = relationOps.columnIndex(sup, mapCol)
        relationOps.filter(sup) { row => mapOps.contains(row(mapIdx), row(keyIdx)) }
      }
    case MapContains(map, key) => // iterate over keys
      val keyCol = extractVarName(key).get.name
      val mapCol = evalTerm(map)
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        val mapIdx = relationOps.columnIndex(sup, mapCol)

        relationOps.flatMap(sup) { row =>
          val keyValues = mapOps.keyIter(row(mapIdx)).toSeq
          relationOps.make(columnsBefore :+ keyCol, keyValues.map(v => row :+ v))
        }
      }
    case _ => super.evalAtomOpen(at)
