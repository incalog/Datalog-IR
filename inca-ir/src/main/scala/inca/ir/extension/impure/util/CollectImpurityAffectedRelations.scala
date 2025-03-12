package inca.ir.extension.impure.util

import inca.ir
import inca.ir.{Atom, Call, Name, Relation}
import inca.ir.extension.aggregate.Aggregate
import inca.ir.extension.impure.{Impure, ImpurityKind}
import inca.ir.visitors.IRVisitor

/** Transitively collect all relations affected by impurities */
class CollectImpurityAffectedRelations extends IRVisitor:
  var affectedRelations: Map[ImpurityKind, Set[Name]] = Map()
  private var affectedMainRelations: Map[ImpurityKind, Set[Name]] = Map()
  private var currentRelation: Relation = _

  private def addAffectedRelation(rel: Name, kind: ImpurityKind): Unit =
    val previousAffectedRelations = affectedRelations.getOrElse(kind, Set())
    affectedRelations += kind -> (previousAffectedRelations + rel)

  /*private def addAffectedMainRelation(rel: Name, kind: ImpurityKind): Unit =
    val previousAffectedRelations = affectedMainRelations.getOrElse(kind, Set())
    affectedMainRelations += kind -> (previousAffectedRelations + rel)*/

  override def visitModule(module: ir.Module): ir.Module =
    affectedRelations = Map()
    //affectedMainRelations = Map()

    var previousAffectedRelations: Map[ImpurityKind, Set[Name]] = Map()
    val mod: ir.Module = super.visitModule(module)
    // fixpoint computation
    while (previousAffectedRelations != affectedRelations) {
      previousAffectedRelations = affectedRelations
      super.visitModule(module)
    }
    // Main relations are affected, but not transitively
    //affectedRelations ++= affectedMainRelations
    mod

  override def visitRelation(relation: Relation): Seq[Relation] =
    currentRelation = relation
    super.visitRelation(relation)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Impure(_, _, _, kind) => //if !currentRelation.hasHint(MainHint) =>
      addAffectedRelation(currentRelation.name, kind)
      super.visitAtom(atom)

    /*case Impure(_, _, _, kind) if currentRelation.hasHint(MainHint) =>
      addAffectedMainRelation(currentRelation.name, kind)
      super.visitAtom(atom)*/

    case Call(ref, args, neg) =>
      affectedRelations.foreach { (kind, rels) =>
        if (rels.contains(ref.name))
          addAffectedRelation(currentRelation.name, kind)
      }
      super.visitAtom(atom)

    case Aggregate(ref, args, op) =>
      affectedRelations.foreach { (kind, rels) =>
        if (rels.contains(ref.name))
          addAffectedRelation(currentRelation.name, kind)
      }
      super.visitAtom(atom)

    case _ => super.visitAtom(atom)
