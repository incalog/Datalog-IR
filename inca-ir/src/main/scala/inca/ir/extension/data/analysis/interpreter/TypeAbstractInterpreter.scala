package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.TypeValue
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference, TData}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  override val dataOps: DataOps[Value, AbstractRelation] = new DataOps[Value, AbstractRelation]:

    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      val ty = caseDef.data
      ty.ref.target = Some(caseDef.data.ref.target.get)
      TypeValue(ty)

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => AbstractRelation)(notMatching: => AbstractRelation): AbstractRelation = v match
        case TypeValue(tdata: TData) if tdata == caseDef.data =>
          effects.joinComputations {
            matching(caseDef.args.map(TypeValue.apply))
          } {
            notMatching
          }
        case _ => notMatching

    override def deconstructNeg(v: Value, caseDef: CaseDefinitionReference)(possibleSuccess: Seq[Value] => AbstractRelation)(success: => AbstractRelation): AbstractRelation = v match
      case TypeValue(tdata: TData) if tdata == caseDef.data =>
        effects.joinComputations {
          possibleSuccess(caseDef.args.map(TypeValue.apply))
        } {
          success
        }
      case _ => success

