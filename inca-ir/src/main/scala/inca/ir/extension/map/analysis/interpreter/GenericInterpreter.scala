package inca.ir.extension.map.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapFun, MapLit, MapLookUp, MapPlus, MapUnion, TMap}
import inca.ir.*
import inca.ir.analysis.base.effect.EmptySupplementary
import inca.ir.extension.demand.TDemand
import inca.ir.extension.set as setir
import sturdy.data.{MakeJoined, MayJoin, mapJoin}

trait MapOps[V, RV, B]:
  def mapLit(vs: Seq[(V, V)]): V
  def mapFun(mapId: Int, f: V => Set[V]): V
  def concat(m1: V, m2: V): V
  def union(ts: Seq[V]): V
  def plus(m: V, k: V, v: V): V
  def lookup(m: V, k: V)(foundValues: Set[V] => RV)(noValuesOrKeyNotFound: => RV): RV
  // Check if key is contained in the map m
  def contains(m: V, key: V): B
  // iterate over the values of the keys of a map if any exists
  def keyIter(m: V)(keySet: Set[V] => RV)(noKeys: => RV): RV


trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]
  with setir.analysis.interpreter.GenericInterpreter[V, B, RV, ExcV, J]: // needed for MapFrom

  lazy val mapOps: MapOps[V, RV, B]

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

  private inline def evalTermTuple(tup: (ir.Term, ir.Term))(using Fixed): (SupColumn, SupColumn) =
    (evalTerm(tup._1), evalTerm(tup._2))

  override protected def canDetermineValue(t: Term): Boolean = t match
    case _: MapComprehension => true
    case _: MapFun => true
    case _: MapFrom => true
    case MapLit(ts) => ts.forall((k, v) => canDetermineValue(k) && canDetermineValue(v))
    case MapUnion(t1, t2) => canDetermineValue(t1) && canDetermineValue(t2)
    case MapConcat(t1, t2) => canDetermineValue(t1) && canDetermineValue(t2)
    case MapPlus(m, k, v) => canDetermineValue(m) && canDetermineValue(k) && canDetermineValue(v)
    case MapLookUp(m, k) => canDetermineValue(m) && canDetermineValue(k)
    case _ => super.canDetermineValue(t)

  /**
   * `f` should compute the mapFun value(s) and return the output columns of the supplementary
   * that contain these value. This function will automatically pack the values into a tuple.
   */
  private def mapFunResult(mapId: Int, inputCols: Seq[String])(f: => Seq[String]): SupColumn =
    // IMPORTANT: snapshot the supplementary at MapFun creation time
    val sup = supplementaryTable.getTable
    val mapFun = mapOps.mapFun(mapId, key => {
      scopedSupplementary { _ =>
        // Key must be a tuple or a single value!
        // Otherwise, operations such as MapLookup are not type correct.
        val keys = tupleOps.iter(key)

        val evalContext =
          if (keys.nonEmpty)
            if (inputCols.size != keys.size)
              throw IllegalStateException(s"MapFun requires exactly ${inputCols.size} many inputs")
            relationOps.naturalJoin(relationOps.make(inputCols, Seq(keys)), sup)
          else
            sup
        supplementaryTable.setTable(evalContext)

        // update the supplementary table
        val outCols = f

        val newSup = supplementaryTable.getTable
        val outputRows = relationOps.extract(newSup, outCols)
        val vs = outputRows.map { v =>
          if (v.size == 1) v.head
          else tupleOps.tupleLit(v)
        }
        vs.toSet
      }
    })
    termResult(mapFun)

  private var mapIds: Map[((Type, Type), Term), Int] = Map()
  private var id: Int = 0
  private def freshId() =
    val lastId = id
    id += 1
    lastId
  private def getUniqueId(term: ir.Term): Int =
    val (kTy, vTy) = term.typ.getOrElse(
      throw new IllegalStateException(s"Require type information for MapFun analysis $term")
    ).ty match
      case TMap(keyTy, valTy) => (keyTy, valTy)
      case ty => throw new IllegalStateException(s"Expected map type for $term but it has type $ty")
    mapIds.get(((kTy, vTy), term)) match
      case Some(id) => id
      case _ =>
        val fresh = freshId()
        mapIds += ((kTy, vTy), term) -> fresh
        fresh

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case MapLit(ts) => naryTupleOp(ts.map(evalTermTuple))(mapOps.mapLit)
    case MapConcat(t1, t2) => binaryOp(evalTerm(t1), evalTerm(t2))(mapOps.concat)
    case MapPlus(map, key, value) => ternaryOp(evalTerm(map), evalTerm(key), evalTerm(value))(mapOps.plus)
    case MapUnion(t1, t2) => naryOp(Seq(t1, t2).map(evalTerm))(mapOps.union)
    case MapFun(params, valTerm) =>
      mapFunResult(getUniqueId(term), params.map(_.name.name))(Seq(evalTerm(valTerm)))
    case MapComprehension(key, value, atoms) =>
      val resultColumn = gensym.fresh("result")
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        except.tryCatch {
          evalAtomGroup(atoms)
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
      val params = relationParams(r)
      val inputIndices = params.zipWithIndex.collect {
        case (Param(_, TDemand(_)), i) => i
      }

      if (inputIndices.isEmpty)
        // no demanded arguments, that means we get a set from the relation
        evalRelationToSet(r)
      else
        // demanded arguments, that means we create a map fun
        val inputCols = inputIndices.map(_ => gensym.fresh("bArg"))
        mapFunResult(getUniqueId(term), inputCols) {
          val argCols = params.indices.map {
            case i if inputIndices.contains(i) => inputCols(i)
            case _ => gensym.fresh("fArg") // fresh arguments are the output of the mapFun
          }
          val args = argCols.map(c => Var(c).arg)
          evalCall(r, params, args, false)
          val outCols = argCols.filter(!inputCols.contains(_))
          outCols
        }

    case MapLookUp(map, key) =>
      val resName = gensym.fresh("result")
      val mapCol = evalTerm(map)
      val keyCol = evalTerm(key)
      updateSupplementaryUnchecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        val mapIx = relationOps.columnIndex(sup, mapCol)
        val keyIx = relationOps.columnIndex(sup, keyCol)
        relationOps.flatMap(sup) { row =>
          val m = row(mapIx)
          val k = row(keyIx)

          mapOps.lookup(m, k) { vs =>
            mapJoin(vs, { v =>
              relationOps.make(columnsBefore :+ resName, Seq(row :+ v))
            })
          } {
            // No values for this key or the key was not found
            except.throws(EmptySupplementary)
          }
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
      val info = extractBindingInfo(key)
      val mapCol = evalTerm(map)
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        val mapIdx = relationOps.columnIndex(sup, mapCol)

        relationOps.flatMap(sup) { row =>
          val m = row(mapIdx)
          val tmpRes = gensym.fresh("result")

          val tmpSup = mapOps.keyIter(m) { keyValues =>
            mapJoin(keyValues, { v =>
              relationOps.make(columnsBefore :+ tmpRes, Seq(row :+ v))
            })
          } {
            // No member is bound, that is, the body fails
            except.throws(EmptySupplementary)
          }

          // bind or check the member columns
          process(tmpSup, info, tmpRes)
        }
      }
    case _ => super.evalAtomOpen(at)
