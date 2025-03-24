package inca.ir.extension.set.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.set.analysis.interpreter.ConstantSetV
import inca.ir.extension.set as irset
import inca.ir.*
import inca.ir.extension.set.TSet
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def eqsToBindConstantParams(body: Body): Seq[Eq] =
    getBodyResult(body).headOption match
      case None => Seq()
      case Some(constRel) =>
        constRel.cols.zip(constRel.rows).flatMap {
          case (c, v: ConstantSetV) =>
            // We must be careful to prevent equality constraints between sets.
            // Sets carry a unique call-site based id, not a structural one. Therefore, equalities will fail.
            val bodyBindsVar = body.vars.map(_.name.name).contains(c)
            if (bodyBindsVar)
              None
            else
              valueToTerm(v).flatMap { t =>
                val expectedTy = params.get(RefByName(Name(c)))
                expectedTy.map(ty => Eq(Var(Name(c)), Cast(t, ty)))
              }
          case (c, v) =>
            valueToTerm(v).flatMap { t =>
              val expectedTy = params.get(RefByName(Name(c)))
              expectedTy.map(ty => Eq(Var(Name(c)), Cast(t, ty)))
            }
        }

  /*private var allowsVariableElimination: Boolean = true

  def allowElimination[A](allow: Boolean)(f: => A): A = {
    val oldAllowSetElimination = this.allowsVariableElimination
    this.allowsVariableElimination = allow
    try {
      val a = f
      a
    } finally {
      this.allowsVariableElimination = oldAllowSetElimination
    }
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case _: Eq | _: Call | _: ExtensionalCall => allowElimination(false) {
      println(s"Disallow: $atom")
      super.visitAtom(atom)
    }
    case _ => allowElimination(true) {
      println(s"Allow: $atom")
      super.visitAtom(atom)
    }*/

  override def mayEliminate(t: Term): Boolean = t match
    case _: irset.SetComprehension => false // contains atoms that might fail
    case _: irset.SetFrom => true
    case irset.SetLit(ts) => ts.forall(mayEliminate)
    case irset.SetUnion(ts) => ts.forall(mayEliminate)
    case irset.SetIntersection(t1, t2) => mayEliminate(t1) && mayEliminate(t2)
    // Do not eliminate set variables if they are binding
    case v: Var if v.typ.exists(tty => tty.ty.isInstanceOf[TSet] && tty.mode.isBinding) => false
    case _ => super.mayEliminate(t)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantSetV(values) => Some(irset.SetLit(values.toSeq.flatMap(valueToTerm.apply)))
    case _ => super.valueToTermInternal(value)

  /*override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case irset.SetMember(mem, s) if mem.typ.exists(_.mode.isBound) => ???
      case irset.SetMember(mem, s) if mem.typ.exists(_.mode.isBinding) => ???
      case _ => super.visitAtom(atom)
  }*/



