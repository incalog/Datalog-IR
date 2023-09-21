package inca.ir.visitors
import inca.ir.{Term, Var}
import collection.mutable

class VarCollector extends IRVisitor:
  private val vars: mutable.Set[Var] = mutable.Set()
  def get: Set[Var] = vars.toSet
  override def visitTerm(term: Term): Seq[Term] = term match
    case v: Var => 
      vars += v
      super.visitTerm(term)
    case _ => 
      super.visitTerm(term)