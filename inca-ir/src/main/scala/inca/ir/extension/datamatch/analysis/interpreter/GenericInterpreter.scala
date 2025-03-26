package inca.ir.extension.datamatch.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.data.{CaseDefinition, CaseDefinitionReference, Construct, DataDefinition, DataDefinitionReference, Deconstruct}
import inca.ir.extension.datamatch.{Case, Match}
import inca.ir.{Atom, Var, RefByName, Name}
import inca.ir.extension.data.analysis.interpreter.DataOps
import inca.ir.visitors.IRVisitor
import sturdy.data.{MakeJoined, MayJoin, mapJoin}

class PatternVarCollector extends IRVisitor:
  var patternVars: Seq[Var] = Seq()

  def extractPatternVars(atom: Atom): Seq[Var] =
    visitAtom(atom)
    patternVars

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Match(_, cases) =>
      patternVars ++= cases.flatMap(_.patVars)
      super.visitAtom(atom)
    case _ => super.visitAtom(atom)

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val dataOps: DataOps[V, RV]

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    // This is basically a disjunction with a deconstruct as first atom
    case Match(matchee, cases) =>
      val dataCol = evalTerm(matchee)

      val supBefore = snapshotSupplementary()
      val colsBefore = relationOps.columns(supBefore)

      // all variables that are in scope after the match construct
      val patVars = PatternVarCollector().extractPatternVars(at).map(_.name.name)
      val caseVars = cases.map(_.vars.map(_.name.name))
      val allVars = caseVars.flatten.intersect(patVars)
      val boundAfterMatch = caseVars.foldLeft[Seq[String]](allVars) { (acc, altVars) =>
        acc.intersect(altVars)
      } ++ colsBefore

      val joinedRes = mapJoin(cases, { case Case(caseRef, patVars, atoms) =>
        supplementaryTable.scoped {
          val caseDef = caseRef.target.get
          val deconNames = caseDef.args.map(_ => gensym.fresh(s"Decon"))
          val deconCols = dataCol +: deconNames

          if (caseDef.args.size != patVars.size)
            throw IllegalArgumentException(s"Case ${caseRef.name} must provide a pattern variable for each argument")

          // 1. Apply the deconstruct operation
          updateSupplementaryChecked { sup =>
            val dataIx = relationOps.columnIndex(supBefore, dataCol)
            relationOps.flatMap(supBefore) { row =>
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
          deconVars.zip(patVars) foreach {
            case (deconVar, t@Var(ref)) => evalEq(deconVar, t, neg = false)
          }

          // 2. Eval the body
          val updatedSup = updateSupplementaryChecked { _ =>
            evalAtomGroup(atoms)
            supplementaryTable.getTable
          }

          // 3. Project relevant vars
          relationOps.project(updatedSup, boundAfterMatch)
        }
      })
      println(s"The joined: $joinedRes")
      updateSupplementaryChecked(_ => joinedRes)
    case _ => super.evalAtomOpen(at)

