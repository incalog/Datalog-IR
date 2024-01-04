package inca.ir.extension.mono

import inca.ir.{Atom, BaseIR, Call, Eq, Name, Var}
import inca.ir.analysis.{BaseIROptimizer, IRAbstractInterpreter}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.set.{SetComprehension, TSet, IR as setIR}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.aggregate.IR as aggregateIR
import inca.ir.extension.demand.DemandIgnoreCallHint
import inca.ir.extension.foreign.ConvertIRForeign
import inca.util.Gensym

/* Note: having many unexpected exceptions */
//trait Optimizer extends BaseIROptimizer:
//  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
//    case Aggregate(rel, args, op@MonoAggregationOperator(monoDef)) if monoDef.isInstanceOf[SetMonoDefinition] =>
//      val Seq((aggVar, aggIdx)) = args.zipWithIndex.flatMap {
//        case (AggregateColumnArg(Var(v)), i) => Some((Var(v), i))
//        case _ => None
//      }
//      val tmpVar = Var(Name(aggVar.name.name + "$tmp"))
//      val callArgs = args.updated(aggIdx, tmpVar.arg)
//      callArgs.foreach {
//        case AggregateColumnArg(_) => throw new IllegalStateException(s"Does not support multiple aggregate columns: $atom")
//      }
//      val callAtom = Call(rel.name, callArgs)
//      val setMono = monoDef.asInstanceOf[SetMonoDefinition]
//      val state = SetComprehension(setMono.addMap(tmpVar), Seq(callAtom))
//      Seq(Eq(aggVar, state))
//    case _ => super.visitAtom(atom)
//  }


trait Optimizer extends BaseLowering:
  override def name: String = "Set mono optimization"

  override def requiredIRs: Set[BaseIR] = Set(setIR, aggregateIR)
  override def loweredIRs: Set[BaseIR] = Set(aggregateIR)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
//    case Aggregate(rel, args, MonoAggregationOperator(sm: SetMonoDefinition2)) =>
//      val Seq((aggVar, aggIdx)) = args.zipWithIndex.flatMap {
//        case (AggregateColumnArg(Var(v)), i) => Some((Var(v), i))
//        case _ => None
//      }
//      val tmp = Name(aggVar.name.name + "$tmp")
//      val callArgs = args.updated(aggIdx, Var(tmp).arg)
//      callArgs.foreach {
//        case AggregateColumnArg(_) => throw new IllegalStateException(s"Does not support multiple aggregate columns: $atom")
//        case _ =>
//      }
//      val callAtom = Call(rel.name, callArgs).addHint(DemandIgnoreCallHint)
//      val set = SetComprehension(Var(tmp), Seq(callAtom))
//      Seq(Eq(aggVar, ConvertIRForeign(set, sm.rtSet, TSet(sm.ty))))

    /**
     * Map Mono Optimization sketch:
     *
     * Coll(mm: MapMono, kv: TDemand(TTuple(K,V))) = nil
     * Agg(mm: MapMono, m: TMap(K,V)) = aggregate(Call(Coll(mm, #m)), mm.monoOp)
     * ~>
     * Coll(mm: MapMono, kv: TDemand(TTuple(K,V))) = nil
     * Coll$split(mm: MapMono, k: TDemand(K), v: TDemand(V)) = Coll(mm, kv), k == kv._1, v == kv._2
     * Agg(mm: MapMono, m: TMap(K,V)) =
     *   m == {(k,v) |
     *     Coll$split(mm, k, _),
     *     aggregate(Call(Coll$split(mm, k, #v)), mm.valMono.monoOp)
     *   }
     *
     * // for nested map monos
     * Agg(mm: MapMono, m: TMap(K,V)) =
     *   m == {(k,v) |
     *     Coll$split(mm, k, _),
     *     v == {(k2,v2) |
     *       Coll$split$split(mm, k, k2, _),
     *       aggregate(Call(Coll$split(mm, k, k2, #v2)), mm.valMono.monoOp)
     *     }
     *   }
     *
     */


    case Aggregate(rel, args, op@MonoAggregationOperator(monoDef)) if monoDef.isInstanceOf[SetMonoDefinition] =>
      val Seq((aggVar, aggIdx)) = args.zipWithIndex.flatMap {
        case (AggregateColumnArg(Var(v)), i) => Some((Var(v), i))
        case _ => None
      }
      val tmp = Name(aggVar.name.name + "$tmp")
      val callArgs = args.updated(aggIdx, Var(tmp).arg)
      callArgs.foreach {
        case AggregateColumnArg(_) => throw new IllegalStateException(s"Does not support multiple aggregate columns: $atom")
        case _ =>
      }
      val callAtom = Call(rel.name, callArgs).addHint(DemandIgnoreCallHint)
      val setMono = monoDef.asInstanceOf[SetMonoDefinition]
      val state = SetComprehension(setMono.addMap(Var(tmp)), Seq(callAtom))
      Seq(Eq(aggVar, state))
    case _ => super.visitAtom(atom)
  }


