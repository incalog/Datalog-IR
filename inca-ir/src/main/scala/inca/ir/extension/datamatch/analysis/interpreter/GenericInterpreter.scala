package inca.ir.extension.datamatch.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.data.CaseDefinition
import inca.ir.extension.datamatch.{Case, Match}
import inca.ir.{Atom, Var, RefByName, Name}
import inca.ir.extension.data.analysis.interpreter.DataOps
import sturdy.data.{MakeJoined, MayJoin, mapJoin}

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val dataOps: DataOps[V, RV]

  private def deconstruct(dataCol: SupColumn, caseDef: CaseDefinition, args: Seq[Var])(using rec: Fixed): Unit =
    val deconNames = caseDef.args.map(_ => gensym.fresh(s"Decon"))
    val deconCols = dataCol +: deconNames

    updateSupplementaryChecked { sup =>
      val dataIx = relationOps.columnIndex(sup, dataCol)
      relationOps.flatMap(sup) { row =>
        val v = row(dataIx)
        dataOps.deconstruct(v, caseDef) {
          vs => relationOps.make(deconCols, Seq(v +: vs))
        } {
          relationOps.make(deconCols, Seq())
        }
      }
    }

    // Assert equalities between deconNames columns and args
    val deconVars = deconNames.map(n => Var(RefByName(Name(n))))
    deconVars.zip(args) foreach {
      case (deconVar, t@Var(ref)) => evalEq(deconVar, t, neg = false)
    }

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    // This is basically a disjunction with a deconstruct as first atom
    case Match(matchee, cases) =>
      val dataCol = evalTerm(matchee)

      updateSupplementaryChecked { supBefore =>
        val colsBefore = relationOps.columns(supBefore).toSet
        val boundAfterMatch = (at.commonVars.map(_.name.name) ++ colsBefore).toSeq

        mapJoin(cases, { case Case(caseRef, patVars, atoms) =>
          scopedSupplementary { _ =>
            val caseDef = caseRef.target.get
            if (caseDef.args.size != patVars.size)
              throw IllegalArgumentException(s"Case ${caseRef.name} must provide a pattern variable for each argument")

            // 1. Apply the deconstruct operation
            deconstruct(dataCol, caseDef, patVars)
            // 2. Eval the body
            evalAtomGroup(atoms)
            // 3. Project relevant vars
            relationOps.project(supplementaryTable.getTable, boundAfterMatch)
          }
        })
      }
    case _ => super.evalAtomOpen(at)

