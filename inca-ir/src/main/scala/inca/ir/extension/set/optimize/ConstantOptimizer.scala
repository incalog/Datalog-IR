package inca.ir.extension.set.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.set.analysis.interpreter.ConstantSetV
import inca.ir.extension.set as irset
import inca.ir.*
import inca.ir.extension.set.TSet
import inca.ir.optimize.ConstantBaseIROptimizer

// We must be careful to prevent equality constraints between sets.
// Sets carry a unique call-site based id, not a structural one. Therefore, equalities will fail.
// To not change the semantics, we do not replace constant set variables if they are binding.
// Otherwise, we would effectively replace a valid assigment with an invalid comparison.
trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def eqsToBindConstantParams(body: Body): Seq[Eq] =
    getBodyResult(body).headOption match
      case None => Seq()
      case Some(constRel) =>
        constRel.cols.zip(constRel.rows).flatMap { (c, v) =>
          val isSet = v.isInstanceOf[ConstantSetV]
          val bodyBindsVar = body.vars.map(_.name.name).contains(c)
          if (isSet && bodyBindsVar)
            None
          else
            super.eqsToBindConstantParams(body)
        }

  override def mayEliminate(t: Term): Boolean = t match
    case irset.SetComprehension(tt, ats) => isConstant(t) && ats.flatMap(visitAtom).isEmpty
    case irset.SetFrom(_) => isConstant(t)
    case irset.SetLit(ts) => isConstant(t) && ts.forall(mayEliminate)
    case irset.SetUnion(ts) => isConstant(t) && ts.forall(mayEliminate)
    case irset.SetIntersection(t1, t2) => isConstant(t) && mayEliminate(t1) && mayEliminate(t2)
    case v: Var if v.typ.exists(tty => tty.ty.isInstanceOf[TSet] && tty.mode.isBinding) => false
    case _ => super.mayEliminate(t)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantSetV(values) => Some(irset.SetLit(values.toSeq.flatMap(valueToTerm.apply)))
    case _ => super.valueToTermInternal(value)



