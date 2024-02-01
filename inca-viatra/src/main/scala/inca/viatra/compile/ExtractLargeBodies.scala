package inca.viatra.compile

import inca.foreign.scala.ir.primitive
import inca.ir
import inca.ir.{Body, Call, Name, Relation, Var}
import inca.ir.visitors.IRVisitor
import inca.util.Gensym

/**
 * Method bodies must not be larger than 64kb in Java. This rewriting uses a simple heuristic based on the number of
 * atoms to find large bodies and extract them to a new relation. That way, a new method is created for this relation
 * and the original relations (and method) shrinks in size.
 */
trait ExtractLargeBodies extends IRVisitor with primitive.Visitor:
  // The maximum number of atoms a pattern can contain
  val maxNumAtoms = 600

  val gensym = new Gensym(Seq.empty)

  override def visitModule(module: ir.Module): ir.Module =
    val mod = super.visitModule(module)
    val (relations, noneRelationContent) = mod.contents.partition(_.isInstanceOf[Relation])
    gensym.register(relations.map(_.name.name))
    mod.copy(mod.name, mod.lang, noneRelationContent ++ relations)

  override def visitRelation(relation: Relation): Seq[Relation] =
    val Seq(rel) = super.visitRelation(relation)

    val remainingBodies :: groupedBodies = rel.bodies.sortBy(_.atoms.size).foldLeft(Seq(Seq.empty[Body])) {
      case (acc, body) =>
        val currentBin = acc.last
        val currentBinSize = currentBin.map(_.atoms.size).sum
        val bodySize = body.atoms.size

        if (currentBinSize + bodySize <= maxNumAtoms)
          acc.init :+ (currentBin :+ body)
        else
          acc :+ Seq(body)
    }

    val name = rel.name
    val params = rel.params

    val newRel = groupedBodies.map(bs => Relation(gensym.freshName(name), params, bs))
    val paramArgs = params.map(p => Var(p.name).arg)
    val newBodies = newRel.map(p => Body(Seq(Call(p.name, paramArgs))))
    Relation(name, params, remainingBodies ++ newBodies) +: newRel

