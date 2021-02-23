package inca.backend.transform

import inca.backend.ir.GP._
import inca.frontend_old.core.CompileToGP.BodyMustFail

object UndefElimination extends Transformation {
  override def transformer: Transformer = new Transformer {
    override def transformBody(body: Body, pat: Pattern): Seq[Body] = {
      val undefs = undefVars(body.constraints)
      if (undefs.isEmpty)
        Seq(body)
      else {
        val cons = eliminateUndefVars(undefs.toSet, body.constraints)
        Seq(Body(cons).withHints(body))
      }
    }

    private def undefVars(cons: Seq[Constraint]): Set[Var] = {
      val undefsCons = cons.collect { case c: Undef => c }
      var undefs = undefsCons.map {
        case Undef(v: Var) => v
        case Undef(c: Constant) => throw BodyMustFail
      }.toSet

      if (undefs.isEmpty)
        return Set()

      val eqs = cons.collect { case eq@Compare(EqComparator, _, _) => eq }
      var foundNew = true
      while (foundNew) {
        val newUndefs = eqs.flatMap {
          case Compare(_, v1: Var, v2: Var) if undefs.contains(v1) && undefs.contains(v2) => Seq()
          case Compare(_, v1: Var, v2: Var) if undefs.contains(v1) => Seq(v2)
          case Compare(_, v1: Var, v2: Var) if undefs.contains(v2) => Seq(v1)
          case Compare(_, v1: Var, _: Constant) if undefs.contains(v1) => throw BodyMustFail
          case Compare(_, _: Constant, v2: Var) if undefs.contains(v2) => throw BodyMustFail
          case _ => Seq()
        }
        foundNew = newUndefs.nonEmpty
        undefs = undefs ++ newUndefs
      }
      undefs
    }

    private def eliminateUndefVars(undefs: Set[Term], cons: Seq[Constraint]): Seq[Constraint] = cons.flatMap {
      case c@Call(name, args, transitive, neg) =>
        if (!neg && args.exists(a => undefs.contains(a)))
          Seq(Call(name, args, transitive, neg = true).withHints(c))
        else
          Seq(c)
      case c@ExtensionalCall(name, args, neg) =>
        if (!neg && args.exists(a => undefs.contains(a)))
          Seq(ExtensionalCall(name, args, neg = true).withHints(c))
        else
          Seq(c)
      case c@Compare(_, lhs, rhs) =>
        if (undefs.contains(lhs) || undefs.contains(rhs))
          Seq()
        else
          Seq(c)
      case c@HasType(t, typ) =>
        if (undefs.contains(t))
          Seq(NotHasType(t, typ).withHints(c))
        else
          Seq(c)
      case c@NotHasType(_, _) =>
        Seq(c)
      case c@Path(src, srcTy, link, trg, trgTy) => (undefs.contains(src), undefs.contains(trg)) match {
        case (true, true) => Seq()
        case (true, false) => Seq(NoPath(trg, trgTy, link, termIsSource = false).withHints(c))
        case (false, true) => Seq(NoPath(src, srcTy, link, termIsSource = true).withHints(c))
        case _ => Seq(c)
      }
      case c@NoPath(t, ty, link, termIsSource) =>
        Seq(c)
      case c@Computed(lhs, computation) =>
        if (computation.args.exists(a => undefs.contains(a)))
          Seq()
        else if (undefs.contains(lhs)) computation match {
          case _: Evaluation => ??? // must negate the evaluation by testing for nullness
          case _: CountAggregation => throw BodyMustFail // aggregation cannot fail
          case _: CustomAggregation => throw BodyMustFail // aggregation cannot fail
        } else {
          Seq(c)
        }
      case Undef(t) =>
        Seq()
    }
  }
}
