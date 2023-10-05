package inca.backend.util

import inca.Scala
import inca.ir.extension.primitiveScala.{Constant, TScala}
import inca.ir.visitors.IRVisitor
import inca.ir.extension.primitiveScala
import inca.ir.{Body, Module, Relation, Term, Var, name2string}

trait Collector[T] extends IRVisitor with primitiveScala.Visitor {
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

object VarCollector {
  def collectAll(relation: Relation): Seq[String] = {
    val varCollector = new VarCollector()
    varCollector.visitRelation(relation)
    varCollector.get()
  }

  def collectAll(body: Body): Seq[String] = {
    val varCollector = new VarCollector()
    varCollector.visitBody(body)
    varCollector.get()
  }
}

class LitCollector extends Collector[(Scala.Literal[_], TScala)] {
  override def visitTerm(term: Term): Seq[Term] = term match
    case Constant(lit, ty) =>
      collect((lit, ty))
      super.visitTerm(term)
    case _ =>
      super.visitTerm(term)
}

object LitCollector {
  def collectAll(relation: Relation): Seq[(Scala.Literal[_], TScala)] = {
    val litCollector = new LitCollector()
    litCollector.visitRelation(relation)
    litCollector.get()
  }

  def collectAll(body: Body): Seq[(Scala.Literal[_], TScala)] = {
    val litCollector = new LitCollector()
    litCollector.visitBody(body)
    litCollector.get()
  }
}