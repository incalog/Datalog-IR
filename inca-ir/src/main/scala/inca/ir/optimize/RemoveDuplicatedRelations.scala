package inca.ir.optimize

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.Aggregate
import inca.ir.extension.aggregateset.AggregateSet
import inca.ir.typing.Mode
import inca.ir.visitors.IRVisitor

/**
 * Remove duplicated relations that contain the exact same bodies.
 * The demand LoweringWithSupplementaries, might produce multiple relations with different parameters,
 * but the exact same bodies.
 * This optimization will detect these colliding relations, determine the minimum set of parameters,
 * merge these relations together and rewrite all calls accordingly.
 */
trait RemoveDuplicatedRelations extends IRVisitor, Optimizer:
  override def name: String = "RemoveDuplicatedRelations"

  // Detect relations with the exact same body
  var collisionMap: Map[Set[Body], Seq[Relation]] = Map()

  // Original relation name -> (new relation name, mapping from old to new params)
  var rewritingMap: Map[Name, (Name, Seq[Int])] = Map()

  enum Phase:
    case CollectCollisions
    case RewriteCalls

  private var phase: Phase = _

  override def visitModule(module: Module): Module = preserveHints(module) {
    // Detect all colliding relations and remove them all
    phase = Phase.CollectCollisions
    val Module(name, lang, contents) = super.visitModule(module)

    // Insert one relation for each collision
    val newRelations = collisionMap.map {
      case (_, Seq(rel)) => rel
      case (bodies, rels) =>
        val newRelName = rels.head.name
        val allParams = rels.flatMap(_.params).distinct
        rewritingMap ++= rels.map { rel =>
          val paramReordering = allParams.map(rel.params.indexOf)
          rel.name -> (newRelName, paramReordering)
        }.toMap
        Relation(newRelName, allParams, bodies.toSeq)
    }

    // Rewrite all calls, aggregate calls etc.
    val mod = Module(name, lang, contents ++ newRelations)
    phase = Phase.RewriteCalls
    super.visitModule(mod)
  }

  override def visitRelation(relation: Relation): Seq[Relation] = phase match
    case Phase.CollectCollisions =>
      val bodies = relation.bodies.toSet
      collisionMap.get(bodies) match
        case None => collisionMap += bodies -> Seq(relation)
        case Some(rels) =>
          logOptimizationStat("duplicate relation", 1, _+1)
          collisionMap += bodies -> (rels :+ relation)
      Seq()
    case Phase.RewriteCalls =>
      super.visitRelation(relation)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Call(ref, args, neg) => rewritingMap.get(ref.name) match
      case Some((relName, paramMapping)) =>
        val newArgs = paramMapping.map(i => args.lift(i).getOrElse(WildcardArg()))
        Seq(Call(RefByName(relName), newArgs, neg))
      case _ => super.visitAtom(atom)
    case Aggregate(ref, args, op) => rewritingMap.get(ref.name) match
      case Some((relName, paramMapping)) =>
        val newArgs = paramMapping.map(i => args.lift(i).getOrElse(WildcardArg()))
        Seq(Aggregate(RefByName(relName), newArgs, op))
      case _ => super.visitAtom(atom)
    case AggregateSet(ref, args, op) => rewritingMap.get(ref.name) match
      case Some((relName, paramMapping)) =>
        val newArgs = paramMapping.map(i => args.lift(i).getOrElse(WildcardArg()))
        Seq(AggregateSet(RefByName(relName), newArgs, op))
      case _ => super.visitAtom(atom)
    case _ => super.visitAtom(atom)