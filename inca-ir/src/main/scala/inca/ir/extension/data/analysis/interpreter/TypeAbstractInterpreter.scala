package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AType, TypeRelation, Value}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference, TData}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  override val dataOps: DataOps[Value, TypeRelation] = new DataOps[Value, TypeRelation]:

    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      val ty = caseDef.data
      ty.ref.target = Some(caseDef.data.ref.target.get)
      AType(ty)

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => TypeRelation)(notMatching: => TypeRelation): TypeRelation = v match
        case AType(tdata: TData) if tdata == caseDef.data =>
          effects.joinComputations {
            matching(caseDef.args.map(AType.apply))
          } {
            notMatching
          }
        case _ => notMatching

