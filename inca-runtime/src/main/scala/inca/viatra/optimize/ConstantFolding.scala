package inca.viatra.optimize

import inca.viatra.optimize.Optimization.BodyMustFail
import inca.ir.{Atom, Body, Eq, Name, Neq, Relation, Term, Var, name2string}
import inca.viatra.util.VarCollector
import inca.viatra.ir.primitiveScala.Constant

import scala.collection.immutable.MultiSet

object ConstantFolding extends Optimization {
  override val name: String = "ConstantFolding"

  override def optimizer(): Optimizer = new Optimizer {
    private var varCount: MultiSet[String] = MultiSet()
    private var paramNames: Seq[String] = Seq()

    override def visitRelation(relation: Relation): Seq[Relation] =
      paramNames = relation.params.map(_.name.name)
      super.visitRelation(relation)

    override def visitBody(body: Body): Seq[Body] = {
      varCount = MultiSet() ++ VarCollector.collectAll(body) ++ paramNames
      try {
        super.visitBody(body)
      } catch {
        case BodyMustFail => Seq()
      }
    }

    override def visitAtom(atom: Atom): Seq[Atom] = atom match {
      case Eq(v: Var, _) if varCount.get(v.name) == 1 => Seq()
      case Neq(v: Var, _) if varCount.get(v.name) == 1 => Seq()
      case Eq(_, v: Var) if varCount.get(v.name) == 1 => Seq()
      case Neq(_, v: Var) if varCount.get(v.name) == 1 => Seq()
      case Eq(t1, t2) if t1 == t2 => Seq()
      case Eq(c1: Constant[_], c2: Constant[_]) if c1.value != c2.value => throw BodyMustFail
      case Neq(t1, t2) if t1 == t2 => throw BodyMustFail
      case Neq(c1: Constant[_], c2: Constant[_]) if c1.value != c2.value => Seq()
      case _ => Seq(atom)
    }
  }

}
