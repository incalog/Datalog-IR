package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ProvenanceAbstractRelation, ProvenanceV, Value}
import inca.ir.extension.data.CaseDefinitionReference
import sturdy.data.WithJoin
import sturdy.values.{Powerset, Topped}


private val RuntimeDataV: Value = ProvenanceV("runtime.data")


trait ProvenanceAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ProvenanceAbstractRelation, Powerset[BaseIRException], WithJoin]:
  override val dataOps: DataOps[Value, ProvenanceAbstractRelation] = new DataOps[Value, ProvenanceAbstractRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      RuntimeDataV

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)
                            (matching: Seq[Value] => ProvenanceAbstractRelation)
                            (notMatching: => ProvenanceAbstractRelation): ProvenanceAbstractRelation =
      matching(caseDef.args.map(_ => RuntimeDataV))

    override def deconstructNeg(v: Value, caseDef: CaseDefinitionReference)
                               (possibleSuccess: Seq[Value] => ProvenanceAbstractRelation)
                               (success: => ProvenanceAbstractRelation): ProvenanceAbstractRelation =
      possibleSuccess(caseDef.args.map(_ => RuntimeDataV))
