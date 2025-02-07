package inca.ir.extension.aggregate.optimize

import inca.ir
import inca.ir.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  private var paramUsedAsAggregateColumn: Map[Relation, Set[Param]] = Map()
  override def mayEliminate(p: Param)(implicit relation: Relation): Boolean =
    !paramUsedAsAggregateColumn.contains(relation) && super.mayEliminate(p)

  private var relationsUsedInAggregations: Set[Relation] = Set()
  override def relationUsedInAggregation(relation: Relation): Boolean =
    relationsUsedInAggregations.contains(relation)

  override def extractBindingVarRef(arg: Arg): Option[Ref[Var.Target]] = arg match
    case AggregateColumnArg(v@Var(ref)) if v.typ.get.mode.isBinding => Some(v.ref)
    case _ => super.extractBindingVarRef(arg)

  override def argTy(arg: Arg): Type = arg match
    case AggregateColumnArg(t) => t.typ.get.ty
    case _ => super.argTy(arg)

  override def isConstant(arg: Arg): Boolean = arg match
    case AggregateColumnArg(t) => isConstant(t)
    case _ => super.isConstant(arg)

  override def analyzeProgram(modules: Seq[Module]): Unit =
    relationsUsedInAggregations = Set()
    paramUsedAsAggregateColumn = Map()

    modules.foreach { m =>
      m.relations.foreach { (_, r) =>
        r.bodies.foreach(_.atoms.foreach {
          case a@Aggregate(ref, args, op) =>
            val rel = ref.target.get
            val aggIndex = args.indexWhere(_.isInstanceOf[AggregateColumnArg])
            if (aggIndex > 0)
              paramUsedAsAggregateColumn += rel -> (paramUsedAsAggregateColumn.getOrElse(rel, Set()) + rel.params(aggIndex))
            relationsUsedInAggregations += rel
          case _ => // nothing
        })
      }
    }

    super.analyzeProgram(modules)


