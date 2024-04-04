package inca.ir.optimize

import inca.ir.*
import inca.ir.typing.Mode
import inca.ir.visitors.IRVisitor

/**
 * If a cast is from a type to itself, remove it.
 */
trait IdentityCastElimination extends IRVisitor:
  override def name: String = "IdentityCastElimination"

  override def visitTerm(term: Term): Seq[Term] = term match
    case Cast(t, ty) => t.typ match
      case Some(TermType(tty, mode)) if ty == tty => super.visitTerm(t)
      case Some(TermType(tty, mode)) => super.visitTerm(term)
      case _ => throw IllegalStateException(s"Untyped expression $t")
    case _ => super.visitTerm(term)
