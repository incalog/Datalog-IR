package inca.viatra.optimize

import inca.viatra.optimize.Optimization.BodyMustFail
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.visitors.IRVisitor
import inca.ir.*
import inca.viatra.ir.primitiveScala.{Constant, Visitor}

import scala.collection.immutable.{Map, MultiDict, Set}

object ConstantPropagation extends Optimization {
  override val name: String = "ConstantPropagation"

  override def optimizer(): Optimizer = new Optimizer {
    override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
      val unsubstitutable = relation.params.map(_.name.name).toSet
      val newbodies = relation.bodies.flatMap(propagateConstants(_, unsubstitutable))
      Seq(Relation(relation.name, relation.params, newbodies))
    }

    private def propagateConstants(body: Body, extUnsubstitutable: Set[String]): Seq[Body] = {
      var substMap: Map[Var, Constant[_]] = Map()
      var unsubstitutable = extUnsubstitutable
      val atoms = body.atoms.flatMap {
        case Eq(v1: Var, c2: Constant[_]) if !unsubstitutable.contains(v1.name) =>
          substMap += v1 -> c2
          unsubstitutable += v1.name
          None
        case Eq(c1: Constant[_], v2: Var) if !unsubstitutable.contains(v2.name) =>
          substMap += v2 -> c1
          unsubstitutable += v2.name
          None
        case a => Some(a)
      }

      def subst(x: Var): Term = substMap.getOrElse(x, x)

      println(s"Substitute: $substMap")

      ConstantSubstitute(subst).visitBody(Body(atoms))
    }
  }

  class ConstantSubstitute(subst: Var => Term) extends Visitor with IRVisitor:
    override def visitTerm(term: Term): Seq[Term] = term match {
      case v: Var => subst(v) match
        case newVar: Var =>
          if (newVar.typ == null)
            newVar.typ = v.typ
          Seq(newVar)
        case t => Seq(t)
      case _ => super.visitTerm(term)
    }
}