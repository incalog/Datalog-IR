package inca.ir.extension.set.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion}
import inca.ir.*
import inca.ir.analysis.base.effect.EmptySupplementary
import inca.ir.extension.tuple.analysis.interpreter.TupleOps
import sturdy.data.{MakeJoined, MayJoin, mapJoin}

trait SetOps[V, B]:
  def setLit(vs: Seq[V]): V
  def union(ts: Seq[V]): V
  def intersect(ts: Seq[V]): V
  // Check if mem is contained in the set s
  def contains(s: V, mem: V): B
  // Produce an iterable for all values of a set s
  def iter(s: V): Iterable[V]


trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  // SetFrom can produce a Set with tuple values from a relation.
  // That means, we need at least a way to create a TupleLit.
  val tupleOps: TupleOps[V]
  val setOps: SetOps[V, B]

  override protected def canDetermineValue(t: Term): Boolean = t match
    case _: SetComprehension => true
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case SetLit(ts) => naryOp(ts.map(evalTerm))(setOps.setLit)
    case SetUnion(ts) => naryOp(ts.map(evalTerm))(setOps.union)
    case SetIntersection(t1, t2) => naryOp(Seq(t1, t2).map(evalTerm))(setOps.intersect)
    case SetFrom(ref) =>
      val r = ref.target.getOrElse(throw new IllegalStateException(s"Unknown relation ${ref.name}"))
      val resultColumn = gensym.fresh("result")
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        except.tryCatch {
          val params = relationParams(r)
          val accCols = params.map(_ => gensym.fresh("arg"))
          val args = accCols.map(c => ir.TermArg(Var(c)))
          evalCall(r, params, args, false)
          val newSup = supplementaryTable.getTable
          relationOps.groupBy(newSup, accCols, columnsBefore)(columnsBefore :+ resultColumn, {
            case (groupedVals, elemVals) if accCols.size == 1 =>
              groupedVals :+ setOps.setLit(elemVals.flatten)
            case (groupedVals, elemVals) =>
              val tups = elemVals.map(tupleOps.tupleLit)
              groupedVals :+ setOps.setLit(tups)
          })
        } /* catch */ { exec =>
          // FIXME: To be in accordance with the lowering we need to differentiate empty sets based on the type
          //  e.g Set[Int]() != Set[String]() 
          relationOps.map(sup, resultColumn) { _ => setOps.setLit(Seq()) }
        }(using mayJoinRV)
      }
      resultColumn
    case SetComprehension(elem, atoms) =>
      // { elem | if atoms hold }
      val resultColumn = gensym.fresh("result")
      updateSupplementaryChecked { sup =>
        val columnsBefore = relationOps.columns(sup)
        except.tryCatch {
          evalAtoms(atoms)
          val elemCol = evalTerm(elem)
          val newSup = supplementaryTable.getTable
          relationOps.groupBy(newSup, elemCol, columnsBefore)(columnsBefore :+ resultColumn, {
            (groupedVals, elemVals) => groupedVals :+ setOps.setLit(elemVals)
          })
        } /* catch */ { exec =>
          // FIXME: To be in accordance with the lowering we need to differentiate empty sets based on the type
          //  e.g Set[Int]() != Set[String]() 
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
        val columnsBefore = relationOps.columns(sup)
        val setIx = relationOps.columnIndex(sup, setCol)

        relationOps.flatMap(sup) { row =>
          val memValues = setOps.iter(row(setIx)).toSeq

          mapJoin(memValues, { v =>
            relationOps.make(columnsBefore :+ memCol, Seq(row :+ v))
          })
        }
      }
    case _ => super.evalAtomOpen(at)
