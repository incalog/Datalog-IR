package inca.backend.util

import inca.ir.visitors.IRVisitor
import inca.ir.{Module, Relation, Term, Var, name2string}

trait Collector[T] extends IRVisitor {
  private var collection: Seq[T] = Seq()

  protected def collect(ele: T): Unit = collection :+= ele
  def get(): Seq[T] = collection
}

class VarCollector extends Collector[String] {
  override def visitTerm(term: Term): Seq[Term] = term match
    case Var(name) =>
      collect(name)
      super.visitTerm(term)
    case _ =>
      super.visitTerm(term)
}
