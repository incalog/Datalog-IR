package inca.ir.extension.data.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.data.{CaseDefinition, CaseDefinitionReference, Construct, DataDefinition, DataDefinitionReference, Deconstruct}
import inca.ir.*
import sturdy.data.MayJoin

trait DataOps[V, R]:
  def construct(cas: CaseDefinitionReference, args: Seq[V]): V
  def deconstruct(v: V, cas: CaseDefinitionReference)(matching: Seq[V] => R)(notMatching: => R): R
  def deconstructNeg(v: V, cas: CaseDefinitionReference)(possibleSuccess: Seq[V] => R)(success: => R): R

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val dataOps: DataOps[V, RV]

  override protected def canDetermineValue(t: ir.Term): Boolean = t match
    case Construct(_, args) => args.forall(canDetermineValue)
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case Construct(caseRef, args) =>
      val caseDef = caseRef.target.get
      naryOp(args.map(evalTerm))(dataOps.construct(caseDef, _))
    case _ => super.evalTermOpen(term)

  override def evalAtomOpen(at: Atom)(using rec: Fixed): Unit = at match
    case Deconstruct(t, caseRef, args, true) =>
      val caseDef = caseRef.target.get

      if (caseDef.args.size != args.size)
        throw IllegalArgumentException(s"Deconstruct must provide a pattern for each argument")

      val dataCol = evalTerm(t)
      val deconNames = caseDef.args.map(_ => gensym.fresh(s"Decon"))
      val deconCols = dataCol +: deconNames

      updateSupplementaryChecked { sup =>
        val dataIx = relationOps.columnIndex(sup, dataCol)
        val supArgs = Some(dataCol) +: args.map {
          case TermArg(t) => Some(evalTerm(t))
          case WildcardArg() => None
        }
        // supplementary with evaluates args named based on the decon cols
        val newSup = relationOps.projectAndRename(
          supplementaryTable.getTable,
          supArgs.zip(deconCols).flatMap {
            case (Some(supCol), deconName) => Some(supCol -> deconName)
            case _ => None
          }.toMap
        )

        relationOps.flatMap(sup) { row =>
          val dataV = row(dataIx)
          dataOps.deconstructNeg(dataV, caseDef) { vs =>
            val deconRV = relationOps.make(deconCols, Seq(dataV +: vs))
            relationOps.project(
              relationOps.antiJoin(deconRV, newSup),
              Seq(dataCol)
            )
          } {
            relationOps.make(Seq(dataCol), Seq(Seq(dataV)))
          }
        }
      }

    case Deconstruct(t, caseRef, args, false) =>
      val caseDef = caseRef.target.get

      if (caseDef.args.size != args.size)
        throw IllegalArgumentException(s"Deconstruct must provide a pattern for each argument")

      val dataCol = evalTerm(t)
      val deconNames = caseDef.args.map(_ => gensym.fresh(s"Decon"))
      val deconCols = dataCol +: deconNames

      // Insert deconNames as columns in the current supplementary
      updateSupplementaryChecked { sup =>
        val dataIx = relationOps.columnIndex(sup, dataCol)
        relationOps.flatMap(sup) { row =>
          val v = row(dataIx)
          dataOps.deconstruct(v, caseDef) { vs =>
              relationOps.make(deconCols, Seq(v +: vs))
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

