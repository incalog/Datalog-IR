package inca.ir.optimize

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.Aggregate
import inca.ir.extension.aggregateset.AggregateSet
import inca.ir.visitors.IRVisitor
import scala.compiletime.uninitialized

/**
 * Remove duplicated relations that contain the exact same bodies. It also removes bodies of the same relation, that are
 * exactly the same.
 * The demand LoweringWithSupplementaries, might produce multiple relations with different parameters,
 * but the exact same bodies.
 * This optimization will detect these colliding relations, determine the minimum set of parameters,
 * merge these relations together and rewrite all calls accordingly.
 */
trait RemoveDuplicatedRelations extends IRVisitor, Optimizer:
  override def name: String = "RemoveDuplicatedRelations"

  // Detect relations with the exact same body
  var collisionMap: Map[Set[Body], Seq[Relation]] = uninitialized

  // Original relation name -> (new relation name, mapping from old to new params)
  var rewritingMap: Map[Name, (Name, Seq[Int])] = uninitialized

  enum Phase:
    case CollectCollisions
    case RewriteCalls

  private var phase: Phase = uninitialized

  override def visitModule(module: Module): Module = preserveHints(module) {
    var changed = true
    var optimized = module

    // Run in a fixpoint until the result is stable
    while (changed) {
      collisionMap = Map()
      rewritingMap = Map()

      // Detect all colliding relations and remove them all
      phase = Phase.CollectCollisions
      val Module(name, lang, contents) = super.visitModule(optimized)

      // Insert one relation for each collision
      val newRelations: Iterable[ModuleEntry] = collisionMap.flatMap {
        case (bodies, Seq(rel)) => Seq(preserveHints(rel) {
          Relation(rel.name, rel.params, bodies.toSeq)
        })
        case (bodies, rels) =>
          val newRelName = rels.head.name
          val allParams = rels.flatMap(_.params).distinct
          val hasCollidingParamsWithDifferentTypes = allParams.map(_.name).toSet.size != allParams.size
          if (hasCollidingParamsWithDifferentTypes)
            rels.map { rel => preserveHints(rel) {
              Relation(rel.name, rel.params, bodies.toSeq)
            }}
          else
            rewritingMap ++= rels.map { rel =>
              val paramReordering = allParams.map(rel.params.indexOf)
              rel.name -> (newRelName, paramReordering)
            }.toMap
            Seq(preserveHints(rels) {
              Relation(newRelName, allParams, bodies.toSeq)
            })
      }

      // Rewrite all calls, aggregate calls etc.
      val mod = Module(name, lang, contents ++ newRelations)
      phase = Phase.RewriteCalls
      val newMod = super.visitModule(mod)
      changed = optimized != newMod
      optimized = newMod
    }

    optimized
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