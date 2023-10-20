package inca.viatra.util

import inca.foreign.scala.ir.primitive.{ScalaTerm, ScalaAggregationAtom, ScalaType, Visitor}
import inca.foreign.scala.syntax.Scala
import inca.ir.visitors.IRVisitor
import inca.viatra.ir.primitiveScala
import inca.ir.{Atom, Body, Module, Relation, Term, Var, name2string}

private trait Collector[T] extends IRVisitor with Visitor {
  private var collection: Seq[T] = Seq()

  protected def collect(ele: T): Unit = collection :+= ele
  def get(): Seq[T] = collection
}

protected[viatra] class VarCollector extends Collector[String] {
  override def visitTerm(term: Term): Seq[Term] = term match
    case Var(name) =>
      collect(name)
      super.visitTerm(term)
    case _ =>
      super.visitTerm(term)
}

protected[viatra] object VarCollector {
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

protected[viatra] class LitCollector extends Collector[(Scala.Literal[_], ScalaType)] {
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case _: ScalaAggregationAtom => Seq()
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case ScalaTerm(lit: Scala.Literal[_], ty, _) => // Ignore all arguments if the body is a literal
      collect((lit, ty))
      Seq()
      //super.visitTerm(term)
    case ScalaTerm(_, _ , _) =>
      // Do not collect literals that are used as arguments for a scala term
      Seq()
    case _ =>
      super.visitTerm(term)
}

protected[viatra] object LitCollector {
  def collectAll(relation: Relation): Seq[(Scala.Literal[_], ScalaType)] = {
    val litCollector = new LitCollector()
    litCollector.visitRelation(relation)
    litCollector.get()
  }

  def collectAll(body: Body): Seq[(Scala.Literal[_], ScalaType)] = {
    val litCollector = new LitCollector()
    litCollector.visitBody(body)
    litCollector.get()
  }
}