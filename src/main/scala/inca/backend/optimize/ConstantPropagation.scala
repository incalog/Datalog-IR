package inca.backend.optimize

import inca.backend.ir.GP._
import inca.backend.ir.Substitute
import inca.runtime.context.DataModel

object ConstantPropagation extends Optimization {

  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {

    override def optimizePattern(pat: Pattern): Seq[Pattern] = {
      val unsubstitutable = pat.params.map(_.name).toSet
      val newbodies = pat.bodies.map(propagateConstants(_, unsubstitutable))
      Seq(Pattern(pat.vis, pat.name, pat.params, newbodies))
    }

    private def propagateConstants(body: Body, unsubstitutable: Set[Name]): Body = {
      var subst: Map[Var, Constant] = Map()
      body.constraints.foreach {
        case Compare(EqComparator, v1: Var, c2: Constant) if !unsubstitutable.contains(v1.name) =>
          subst += v1 -> c2
        case Compare(EqComparator, c1: Constant, v2: Var) if !unsubstitutable.contains(v2.name) =>
          subst += v2 -> c1
        case _ => // nothing
      }
      Substitute.fromMap(subst).substBody(body)
    }
  }
}
