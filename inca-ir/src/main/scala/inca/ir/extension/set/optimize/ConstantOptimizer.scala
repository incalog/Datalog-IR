package inca.ir.extension.set.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.set.analysis.interpreter.ConstantSetV
import inca.ir.extension.set as irset
import inca.ir.*
import inca.ir.extension.set.{SetMember, TSet}
import inca.ir.optimize.ConstantBaseIROptimizer
import inca.ir.visitors.IRVisitor

// We must be careful to prevent equality constraints between sets.
// Sets carry a unique call-site based id, not a structural one. Therefore, equalities will fail.
// To not change the semantics, we do not replace constant set variables if they are binding.
// Otherwise, we would effectively replace a valid assigment with an invalid comparison.
trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def eqsToBindConstantParams(body: Body): Seq[Eq] =
    getBodyResult(body).headOption match
      case None => Seq()
      case Some(constRel) =>
        val eqs = super.eqsToBindConstantParams(body)
        constRel.cols.zip(constRel.rows).zip(eqs).flatMap { case ((c, v), eq) =>
          val isSet = v.isInstanceOf[ConstantSetV]
          val bodyBindsVar = body.vars.map(_.name.name).contains(c)
          if (isSet && bodyBindsVar)
            None
          else
            Some(eq)
        }

  override def mayEliminate(t: Term): Boolean = t match
    case irset.SetComprehension(tt, ats) => isConstant(t) && ats.flatMap(visitAtom).isEmpty
    case irset.SetFrom(_) => isConstant(t)
    case irset.SetLit(ts) => isConstant(t) && ts.forall(isConstant) && ts.forall(mayEliminate)
    case irset.SetUnion(ts) => isConstant(t) && ts.forall(isConstant) && ts.forall(mayEliminate)
    case irset.SetIntersection(t1, t2) => 
      isConstant(t) && isConstant(t1) && isConstant(t2)
      && mayEliminate(t1) && mayEliminate(t2)
    case v: Var if inSetMember && v.typ.exists(_.mode.isBinding) => false
    case v: Var if v.typ.exists(tty => tty.ty.isInstanceOf[TSet] && tty.mode.isBinding) => false
    case _ => super.mayEliminate(t)

  private var inSetMember: Boolean = false
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case irset.SetMember(mem, s) =>
      // Don't eliminate binding set members
      val oldInSetMember = inSetMember
      inSetMember = true
      val visitedMem = visitTerm(mem)
      inSetMember = oldInSetMember
      visitedMem.zip(visitTerm(s)).map { case (s1, s2) => SetMember(s1, s2) }
    case _ => super.visitAtom(atom)

  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantSetV(values) =>
      val newValues = values.toSeq.flatMap(valueToTerm.apply)
      if (newValues.size == values.size)
        Some(irset.SetLit(newValues))
      else
        None
    case _ => super.valueToTermInternal(value)


  private var relationsUsedInSetFrom: Set[Relation] = Set()

  override def relationIsRequired(relation: Relation): Boolean =
    if (relationsUsedInSetFrom.contains(relation))
      true
    else
      super.relationIsRequired(relation)

  override def analyzeProgram(modules: Seq[Module]): Unit =
    relationsUsedInSetFrom = Set()

    val setFromVisitor = new IRVisitor {
      override def visitTerm(term: Term): Seq[Term] = term match
        case irset.SetFrom(ref) =>
          val rel = ref.target.get
          relationsUsedInSetFrom += rel
          super.visitTerm(term)
        case _ =>
          super.visitTerm(term)
    }

    setFromVisitor.visitProgram(modules)
    super.analyzeProgram(modules)

