package inca.ir.extension.aggregate.optimize

import inca.ir
import inca.ir.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override protected def mayEliminate(arg: Arg): Boolean = arg match
    case AggregateColumnArg(t) => mayEliminate(t)
    case _ => super.mayEliminate(arg)

  private var paramUsedAsAggregateColumn: Map[Relation, Set[Param]] = Map()
  override def mayEliminate(p: Param)(implicit relation: Relation): Boolean =
    !paramUsedAsAggregateColumn.contains(relation) && super.mayEliminate(p)

  private var relationsUsedInAggregations: Set[Relation] = Set()
  override def relationUsedInAggregation(relation: Relation): Boolean =
    relationsUsedInAggregations.contains(relation)

  override def extractBindingVarRef(arg: Arg): Option[Ref[Var.Target]] = arg match
    case AggregateColumnArg(v@Var(ref)) if v.typ.get.mode.isBinding => Some(v.ref)
    //case AggregateColumnArg(_) => None
    case _ => super.extractBindingVarRef(arg)

  override def argTy(arg: Arg): Type = arg match
    case AggregateColumnArg(t) => t.typ.get.ty
    case _ => super.argTy(arg)

  override def isConstant(arg: Arg): Boolean = arg match
    case AggregateColumnArg(t) => false //isConstant(t)
    case _ => super.isConstant(arg)

  /*override def mayEliminate(arg: Arg): Boolean = arg match
    case AggregateColumnArg(t) => false
    case _ => super.isConstant(arg)*/

  override def visitArg(arg: Arg): Seq[Arg] = arg match
    // Do not try to replace terms with constants in AggregateColumnArg
    case AggregateColumnArg(t) => Seq(arg)
    case _ => super.visitArg(arg)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case agg@Aggregate(ref, args, op) =>
      ref.target match
        case Some(r: Relation) =>
          val res = getRelationResult(r).headOption.get
          val eliminatetable = r.params.map(mayEliminate(_)(r))
          val (constantArgs, nonconstantArgs) = args.zip(res.rows).zip(eliminatetable)
            .partition { case ((arg, res), elim) => isConstant(arg) && res.isConstant && elim }
          // nonconstantArgs can never be empty, since we always have an aggregation column
          Seq(agg.copy(args = nonconstantArgs.flatMap { case ((a, _), _) => visitArg(a) }))
        case _ => super.visitAtom(atom)
    case _ => super.visitAtom(atom)

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


