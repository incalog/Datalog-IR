package inca.ir.extension.demand.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.extension.demand.TDemand
import sturdy.data.MayJoin

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
    if (!interRelational)
        println("[WARNING:] Intra-relational demand analysis is not supported!")

    def provideValueForDemandedParam(param: ir.Param): V

    override def evaluationContextForCall[R <: ModuleEntry](r: R, params: Seq[Param], args: Seq[Arg])(using Fixed): (RV, ArgBindingInfo) =
        // This might occur if we query a relation with demanded params without demand (See: DemandIgnoreCallHint)
        // E.g. a field read
        val (evalContext, info) = super.evaluationContextForCall(r, params, args)
        val demandedParams = params.collect { case p if p.ty.isInstanceOf[TDemand] => p }
        val unboundDemandedParams = demandedParams.filter {
            p => !relationOps.hasColumn(evalContext, p.name.name)
        }
        val extendedEvalContext = unboundDemandedParams.foldLeft(evalContext) { (accRv, param) =>
            relationOps.map(accRv, param.name.name)(_ => provideValueForDemandedParam(param))
        }
        (extendedEvalContext, info)
