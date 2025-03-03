package inca.ir.extension.set.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion}
import inca.ir.*
import inca.ir.analysis.base.effect.EmptySupplementary
import sturdy.data.{MayJoin, mapJoin, MakeJoined}

trait SetOps[V, B]:
  def setLit(vs: Seq[V]): V
  def union(ts: Seq[V]): V
  def intersect(ts: Seq[V]): V
  // Check if mem is contained in the set s
  def contains(s: V, mem: V): B
  // Produce an iterable for all values of a set s
  def iter(s: V): Iterable[V]


// TODO: Support ConstantRelationOps

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val setOps: SetOps[V, B]

  override protected def canDetermineValue(t: Term): Boolean = t match
    case _: SetComprehension => true
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case SetLit(ts) => naryOp(ts.map(evalTerm))(setOps.setLit)
    case SetFrom(ref) =>
      // TODO: Implement me
      val r = ref.target.getOrElse(throw new IllegalStateException(s"Unknown relation ${ref.name}"))
      // 1. Eval the call and get all projected columns
      // 2. Use the evaluated RV to construct a set (How to do that?)
      ???
    case SetUnion(ts) => naryOp(ts.map(evalTerm))(setOps.union)
    case SetIntersection(t1, t2) => naryOp(Seq(t1, t2).map(evalTerm))(setOps.intersect)
    case SetComprehension(elem, atoms) =>
      // { elem | if atoms hold }
      val resultColumn = gensym.fresh("result")
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        except.tryCatch {
          evalAtoms(atoms)
          val elemCol = evalTerm(elem)

          // TODO: I think this should be a groupBy on the columns before:
          //  val groupByCols = columnsBefore
          //  groupBy(accumulatorCols: Seq[String], groupByCols: Seq[String])(newCols: Seq[String], f: (groupByCols: Row[V], accValues: Seq[Row[V]]) => Row[V]): RV
          //  groupBy(accumulatorCols: Seq[String], groupByCols: Seq[String])(newCols: Seq[String], {
          //    case (groupedCols: Seq[V], vs: Seq[Seq[V]]) =>  // for each group
          //      val newValues = vs.map(_.apply(0)) // we only have a single element we want to accumulate
          //      groupedCols :+ setOps.setLit(newValues)
          //  })
          //  This should drop all other columns
          //  Concrete impl could look like this:
          //  val rows = Seq(
          //    Seq(1,2,4,5),
          //    Seq(1,2,7,8),
          //    Seq(1,3,4,9)
          //  )
          //  // val groupByIndices = groupByColumns.map(cols.indexOf)
          //  val groupByIndices = Seq(0,1)
          //  val accIndices = Seq(2, 3)
          //  val grouped = rows.groupBy(row => groupByIndices.map(row.apply))
          //  grouped.map { (groupedRows, rows) =>
          //    val accValues = rows.map(row => accIndices.map(row.apply))
          //    println(s"$groupedRows -> $accValues")
          //  }
          val newSup = supplementaryTable.getTable
          val elemColIdx = relationOps.columnIndex(newSup, elemCol)
          val allCols = relationOps.columns(newSup)

          val initial = allCols.indices.map(_ => setOps.setLit(Seq()))
          var resRV = relationOps.fold(newSup, initial) { (acc, row) =>
            val preV = acc(elemColIdx)
            val newV = setOps.setLit(Seq(row(elemColIdx)))
            row.updated(elemColIdx, setOps.union(Seq(preV, newV)))
          }
          resRV = relationOps.copyColumn(resRV, elemCol, resultColumn)
          relationOps.project(resRV, columnsBefore :+ resultColumn)
        } /* catch */ { exec =>
          relationOps.map(sup, resultColumn) { _ => setOps.setLit(Seq()) }
        }(using mayJoinRV)
      }
      resultColumn
    case _ => super.evalTermOpen(term)

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case SetMember(mem, s) if canDetermineValue(mem) => // containment check
      val memCol = evalTerm(mem)
      val setCol = evalTerm(s)
      updateSupplementaryChecked { sup =>
        val memIx = relationOps.columnIndex(sup, memCol)
        val setIx = relationOps.columnIndex(sup, setCol)
        relationOps.filter(sup) { row => setOps.contains(row(setIx), row(memIx)) }
      }
    case SetMember(mem, s) => // iterate over the set
      val memCol = extractVarName(mem).get.name
      val setCol = evalTerm(s)
      updateSupplementaryChecked { sup =>
        val setIx = relationOps.columnIndex(sup, setCol)
        relationOps.flatMap(sup) { row =>
          val memValues = setOps.iter(row(setIx))
          mapJoin(memValues, { value =>
            relationOps.map(sup, memCol) { _ => value }
          })
        }
      }
    case _ => super.evalAtomOpen(at)

