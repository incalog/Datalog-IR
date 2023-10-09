package inca.viatra.optimize

import inca.viatra.optimize.Optimization.BodyMustFail
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, Body, Eq, Neq, Relation, Term, Var, name2string}
import inca.ir.visitors.IRVisitor
import inca.viatra.ir.primitiveScala.Visitor
import inca.viatra.optimize.Optimization.BodyMustFail

import scala.collection.immutable.{Map, MultiDict, Set}

object EliminateAliases extends Optimization {
  override val name: String = "EliminateAliases"

  override def optimizer(): Optimizer = new Optimizer {
    override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
      val unsubstitutable = relation.params.map(_.name.name).toSet
      val newbodies = relation.bodies.flatMap(eliminateAliasesInBody(_, unsubstitutable))
      Seq(Relation(relation.name, relation.params, newbodies))
    }

    private def eliminateAliasesInBody(body: Body, unsubstitutable: Set[String]): Seq[Body] = preserveHints(body) {
      // substMap tracks aliases
      var substMap: Map[Var, Var] = Map()

      def addAlias(from: Var, to: Var): Unit = {
        substMap = substMap.view.mapValues(v => if (v == from) to else v).toMap + (from -> to)
      }

      def addAliases(vars: Set[Var]): Boolean = {
        val svars = vars.map(subst)
        val (unsubstVars, substVars) = svars.partition(v => unsubstitutable.contains(v.name))
        if (unsubstVars.nonEmpty) {
          val base = unsubstVars.head
          substVars.foreach(v => addAlias(v, base))
          substVars.nonEmpty
        } else if (substVars.size >= 2) {
          val base = substVars.head
          substVars.tail.foreach(v => addAlias(v, base))
          true
        } else {
          false
        }
      }

      def subst(x: Var): Var = substMap.getOrElse(x, x)

      def substTerm(t: Term): Term = t match {
        case v: Var => substMap.getOrElse(v, v)
        case _ => t
      }

      body.atoms.foreach {
        case Eq(t1, t2) =>
          (substTerm(t1), substTerm(t2)) match {
            case (v1: Var, v2: Var) if !unsubstitutable.contains(v2.name) =>
              addAlias(v2, v1)
            case (v1: Var, v2: Var) if !unsubstitutable.contains(v1.name) =>
              addAlias(v1, v2)
            case _ => // nothing
          }
        case _ => // nothing
      }

      try {
        val substBodies = new AliasSubstitute(subst).visitBody(body)
        substBodies.map(b => Body(b.atoms.distinct))
      } catch {
        case BodyMustFail => Seq()
      }
    }
  }

  class AliasSubstitute(subst: Var => Term) extends Visitor with IRVisitor:
    override def visitAtom(atom: Atom): Seq[Atom] = atom match {
      case Eq(lhs, rhs) =>
        val left = visitTerm(lhs)
        val right = visitTerm(rhs)
        if (left == right)
          Seq()
        else
          super.visitAtom(atom)
      case Neq(lhs, rhs) =>
        val left = visitTerm(lhs)
        val right = visitTerm(rhs)
        if (left == right)
          throw BodyMustFail
        else
          super.visitAtom(atom)
      case _ =>
        super.visitAtom(atom)
    }

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
