package inca.ir.extension.data.analysis.interpreter

import inca.ir
import inca.ir.{Atom, Name, RefByName, TermArg, Var, WildcardArg}
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.data.{Construct, Deconstruct}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.except.Except
import sturdy.effect.failure.Failure
import sturdy.data.MakeJoined
import sturdy.data.CombineUnit

import java.sql.Ref

trait DataOps[V, R]:
  def construct(dataName: String, caseName: String, args: Seq[V]): V
  def deconstruct(v: V, dataName: String, caseName: String)(matching: Seq[V] => R)(notMatching: => R): R

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val dataOps: DataOps[V, RV]

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case Construct(caseRef, args) =>
      val caseDef = caseRef.target.get
      val dataName = caseDef.data.ref.name
      naryOp(args.map(evalTerm))(dataOps.construct(dataName.name, caseDef.name.name, _))
    case _ => super.evalTermOpen(term)

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case Deconstruct(t, caseRef, args, neg) =>
      val caseDef = caseRef.target.get
      val caseName = caseDef.name.name
      val dataName = caseDef.data.ref.name.name

      if (caseDef.args.size != args.size)
        throw IllegalArgumentException(s"Deconstruct must provide a pattern for each argument")

      val dataCol = evalTerm(t)
      val deconNames = caseDef.args.map(_ => gensym.fresh(s"Decon"))
      val deconCols = dataCol +: deconNames

      // Insert deconNames as columns in the current supplementary
      updateSupplementaryChecked { sup =>
        val dataIx = relationOps.columnIndex(sup, dataCol)
        relationOps.flatMap(sup) { row =>
          val data = row(dataIx)
          dataOps.deconstruct(data, dataName, caseName) {
            vs => relationOps.make(deconCols, Seq(data +: vs))
          } {
            relationOps.make(deconCols, Seq())
          }
        }
      }

      // Assert equalities between deconNames columns and args
      val deconVars = deconNames.map(n => Var(RefByName(Name(n))))
      deconVars.zip(args) foreach {
        case (deconVar, TermArg(t)) => evalEq(deconVar, t, neg = false)
        case (deconVar, WildcardArg()) => // skip
      }

    case _ => super.evalAtomOpen(at)

