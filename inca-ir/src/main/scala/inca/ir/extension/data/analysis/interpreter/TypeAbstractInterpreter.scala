package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{TypeRelation, TypeValue}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference, TData}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.{Powerset, Topped}

trait TypeAbstractInterpreter extends GenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:
  override val dataOps: DataOps[TypeValue, TypeRelation] = new DataOps[TypeValue, TypeRelation]:
    override def construct(dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference, args: Seq[TypeValue]): TypeValue = TypeValue.AType(TData(dataDef.name))
    override def deconstruct(v: TypeValue, dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference)(matching: Seq[TypeValue] => TypeRelation)(notMatching: => TypeRelation): TypeRelation = v match
      case TypeValue.AType(tdata: TData) if tdata.ref.target.contains(dataDef) =>
        effects.joinComputations {
          matching(caseDef.args.map(TypeValue.AType.apply))
        } {
          notMatching
        }
      case _ => notMatching

