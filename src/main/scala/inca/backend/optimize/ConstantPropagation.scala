package inca.backend.optimize

import inca.backend.ir.Datalog._
import inca.backend.ir.GeneratePSystem
import inca.backend.ir.Substitute
import inca.runtime.context.DataModel
import inca.util.Scala

object ConstantPropagation extends Optimization {

  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {

    override def optimizePattern(pat: Pattern): Seq[Pattern] = {
      val unsubstitutable = pat.params.map(_.name).toSet
      val newbodies = pat.bodies.map(propagateConstants(_, unsubstitutable))
      Seq(Pattern(pat.vis, pat.name, pat.params, newbodies).withHints(pat))
    }

    private def propagateConstants(body: Body, unsubstitutable: Set[Name]): Body = {
      var subst: Map[Var, Constant] = Map()
      val atoms = body.atoms.flatMap {
        case Compare(EqComparator, v1: Var, c2: Constant) if !unsubstitutable.contains(v1.name) =>
          subst += v1 -> c2
          None
        case Compare(EqComparator, c1: Constant, v2: Var) if !unsubstitutable.contains(v2.name) =>
          subst += v2 -> c1
          None
        case a @ Computed(v: Var, Evaluation(_, _, Scala(meta.Term.Function(Nil, lit: meta.Lit))))
            if !unsubstitutable.contains(v.name) =>
          Literal.fromScalaMeta(lit) match {
            case Some(l) =>
              subst += v -> Constant(l)
              None
            case None =>
              Some(a)
          }
        case a => Some(a)
      }
      ConstantSubstitute.fromMap(subst).substBody(Body(atoms).withHints(body))
    }
  }

  class ConstantSubstitute(subst: Var => Term) extends Substitute(subst) {
    override def substComputation(comp: Computation): Computation = comp match {
      case Evaluation(args, resultType, code) =>
        val newargs = args.map(a => substTerm(a._1) -> a._2)
        var scalaConsts: Map[String, meta.Term] = Map()
        val (remainingArgs, remainingParams) = newargs.zip(code.tree.params).flatMap {
          case ((Constant(lit), _), p) =>
            scalaConsts += p.name.value -> GeneratePSystem.genLiteral(lit)
            None
          case ap => Some(ap)
        }.unzip
        val substBody = EvalFusion.scalaSubst(code.tree.body, scalaConsts)
        Evaluation(
          remainingArgs,
          resultType,
          Scala(meta.Term.Function(remainingParams.toList, substBody))
        )
      case _ => super.substComputation(comp)
    }
  }

  object ConstantSubstitute {
    def fromMap(m: Map[Var, Term]) = new ConstantSubstitute(v => m.getOrElse(v, v))
  }

}
