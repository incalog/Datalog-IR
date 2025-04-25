package inca.ir.extension.datamatch.optimize

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.analysis.base.values.Value
import inca.ir.extension.datamatch.{Match, Case}
import inca.ir.optimize.ConstantBaseIROptimizer

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  private var commonMatchVars: Set[Name] = Set()
  def scopedMatchVars[A](m: Match)(f: => A): A = {
    val oldCommonMatchVars = commonMatchVars
    commonMatchVars = m.commonVars.map(_.name)
    try {
      val a = f
      a
    } finally {
      commonMatchVars = oldCommonMatchVars
    }
  }

  override def mayEliminate(at: Atom): Boolean = at match
    case eq: Eq =>
      val matchVarOption = eq.vars.find(v => commonMatchVars.contains(v.name))
      val isBinding = matchVarOption.exists(_.mode.isBinding)
      if (isBinding)
        false
      else
        super.mayEliminate(at)
    case _ => super.mayEliminate(at)

  override def mayEliminate(t: Term): Boolean = t match
    case v@Var(_) if v.mode.isBinding && commonMatchVars.contains(v.name) => false
    case _ => super.mayEliminate(t)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      // we must not eliminate common vars that must be bound after the match block
      case m@Match(matchee, cases) => scopedMatchVars(m)(super.visitAtom(atom))
      case _ => super.visitAtom(atom)
  }



