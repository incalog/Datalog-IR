package inca.ir.extension.data.analysis.interpreter

import inca.ir.Relation
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.interpreter.Adornment
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, BaseMeetV, Value}
import inca.ir.extension.data.{CaseDefinitionReference, TData}
import inca.ir.hints.FoldHint
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.data.WithJoin

// If isClosed is true, it means, that the set of CaseDefinitionReference also applies to all
// children inside the construct. E.g.
//   e ~> { Add(?, ?), Num }
// We don't know anything about the children of Add.
// If we set `isClosed` to true, then we guarantee that the children of add are either
// `Add` or `Num`. They can't be any other kind.
case class DataKindV(caseDefs: Set[CaseDefinitionReference], isClosed: Boolean) extends Value:
  override def toString: String =
    val caseStr = caseDefs.map { c =>
      val argS = c.args.map(_ => if (isClosed) "!" else "?").mkString("(", ",", ")")
      s"${c.name}$argS"
    }
    caseStr.mkString("{", ",", "}")
  override def isConstant: Boolean = caseDefs.size == 1 && caseDefs.head.args.isEmpty

trait DataKindEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (DataKindV(caseDefs1, _), DataKindV(caseDefs2, _)) =>
      val intersection = caseDefs1.intersect(caseDefs2)
      if (intersection.isEmpty)
        Topped.Actual(false)
      else
        Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (DataKindV(caseDefs1, _), DataKindV(caseDefs2, _)) =>
      val intersection = caseDefs1.intersect(caseDefs2)
      if (intersection.isEmpty)
        Topped.Actual(true)
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait DataKindJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (DataKindV(caseDefs1, isClosed1), DataKindV(caseDefs2, isClosed2)) =>
      DataKindV(caseDefs1.union(caseDefs2), isClosed1 && isClosed2)
    case _ => super.join(lhs, rhs)

trait DataKindMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (DataKindV(caseDefs1, isClosed1), DataKindV(caseDefs2, isClosed2)) =>
      val intersect = caseDefs1.intersect(caseDefs2)
      if (intersect.isEmpty)
        throwBotException()
      else
        DataKindV(intersect, isClosed1 && isClosed2)
    case _ => super.meet(lhs, rhs)

trait DataKindAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:

  val dataOps: DataOps[Value, AbstractRelation] = new DataOps[Value, AbstractRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      DataKindV(Set(caseDef), false)

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => AbstractRelation)(notMatching: => AbstractRelation): AbstractRelation = v match
      case DataKindV(caseDefs, false) =>
        if (caseDefs.contains(caseDef))
          effects.joinComputations {
            matching(caseDef.args.map(_ => Value.Top))
          } {
            notMatching
          }
        else
          notMatching
      case DataKindV(caseDefs, true) =>
        if (caseDefs.contains(caseDef))
          val dataRef = caseDefs.head.data
          effects.joinComputations {
            matching(caseDef.args.map { ty =>
              if (ty == dataRef) v
              else Value.Top
            })
          } {
            notMatching
          }
        else
          notMatching
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Value.Top))
        } {
          notMatching
        }

  override def evalRelationOpen(r: Relation, adorn: Adornment)(using Fixed): AbstractRelation =
    val res = super.evalRelationOpen(r, adorn)
    // If we have a fold hint, we can conclude that the shape of the
    // data is always recursively the same
    r.getHint[FoldHint](FoldHint) match
      case Some(FoldHint(resCol)) =>
        val allCols = relationOps.columns(res)
        val tmpCol = gensym.fresh("tmp")
        val renamedRv = relationOps.rename(res, Map(resCol.name -> tmpCol))
        val resColIndex = relationOps.columnIndex(renamedRv, tmpCol)
        val newRes = relationOps.map(renamedRv, resCol.name) { row =>
          row(resColIndex) match
            case DataKindV(caseDefs, false) => DataKindV(caseDefs, true)
            case v => v
        }
        relationOps.project(newRes, allCols)
      case _ =>
        res
