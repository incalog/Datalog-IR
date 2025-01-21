package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{TypeRelation, TypeValue}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference, TData}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  override val dataOps: DataOps[TypeValue, TypeRelation] = new DataOps[TypeValue, TypeRelation]:

    override def construct(caseDef: CaseDefinitionReference, args: Seq[TypeValue]): TypeValue =
      val ty = caseDef.data
      ty.ref.target = Some(caseDef.data.ref.target.get)
      TypeValue.AType(ty)

    override def deconstruct(v: TypeValue, caseDef: CaseDefinitionReference)(matching: Seq[TypeValue] => TypeRelation)(notMatching: => TypeRelation): TypeRelation = v match
        case TypeValue.AType(tdata: TData) if tdata == caseDef.data =>
          effects.joinComputations {
            matching(caseDef.args.map(TypeValue.AType.apply))
          } {
            notMatching
          }
        case _ => notMatching

