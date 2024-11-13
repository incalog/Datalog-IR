package inca.viatra.util

import inca.foreign.scala.ir.primitive.{ScalaAggregationOperator, ScalaConstantTerm, ScalaDefnModuleEntry, ScalaTerm, ScalaType, Visitor}
import inca.ir.extension.aggregate.Aggregate
import inca.ir.visitors.IRVisitor
import inca.ir.{Atom, Body, Module, ModuleEntry, RefByName, Relation, Term, Var, name2string}

private trait Collector[T] extends IRVisitor with Visitor {
  private var collection: Seq[T] = Seq()

  protected def collect(ele: T): Unit = collection :+= ele

  def get(): Seq[T] = collection
}

protected[viatra] class VarCollector extends Collector[String] {
  override def visitTerm(term: Term): Seq[Term] = term match
    case Var(RefByName(name)) =>
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

protected[viatra] class LitCollector extends Collector[(String, ScalaType)] {
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case _: Aggregate => Seq()
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case ScalaConstantTerm(code, ty) =>
      collect((code, ty))
      Seq()
    case ScalaTerm(_, _, _, _) =>
      // Do not collect literals that are used as arguments for a scala term
      Seq()
    case _ =>
      super.visitTerm(term)
}

protected[viatra] object LitCollector {
  def collectAll(relation: Relation): Seq[(String, ScalaType)] = {
    val litCollector = new LitCollector()
    litCollector.visitRelation(relation)
    litCollector.get()
  }

  def collectAll(body: Body): Seq[(String, ScalaType)] = {
    val litCollector = new LitCollector()
    litCollector.visitBody(body)
    litCollector.get()
  }
}

protected[viatra] class ScalaModuleEntryCollector extends Collector[ScalaDefnModuleEntry] {
  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = moduleEntry match
    case defn: ScalaDefnModuleEntry =>
      collect(defn)
      super.visitModuleEntry(moduleEntry)
    case _ =>
      super.visitModuleEntry(moduleEntry)
}

protected[viatra] object ScalaModuleEntryCollector {
  def collectAll(module: Module): Seq[ScalaDefnModuleEntry] = {
    val defnCollector = new ScalaModuleEntryCollector()
    defnCollector.visitProgram(Seq(module))
    defnCollector.get()
  }
}