package inca.ir.extension.demand

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.typing.Mode
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.MultiDict
import scala.collection.immutable.Seq

trait AdornmentAnalysis extends Visitor:

  /** for each relation, the parameters that are demanded (free) */
  var demandedParams: MultiDict[String, String] = MultiDict()

  /** Used to track progress */
  var size: Int = 0

  var currentRelations: Map[String, Relation] = Map()
  var currentRelation: Relation = _

  def addCurrentDemand(v: Var): Unit =
    if (currentRelation.params.map(_.name.name).contains(v.name.name)) {
      if (!demandedParams.containsEntry(currentRelation.name.name -> v.name.name)) {
        demandedParams += currentRelation.name.name -> v.name.name
        size += 1
      }
    } else
      throw new IllegalArgumentException(s"Demanded variable $v is not a parameter of $currentRelation")

  def currentDemand: collection.Set[String] =
    demandOf(currentRelation.name)
  def demandOf(rel: Name): collection.Set[String] =
    demandedParams.get(rel.name)
  def demandPositionsOf(rel: Name): collection.Set[Int] =
    val ps = demandedParams.get(rel.name)
    val r = currentRelations(rel.name)
    val relParams = r.params.map(_.name.name)
    ps.map(relParams.indexOf)

  override def visit(module: Module): Module =
    var oldSize = -1
    var result = module
    currentRelations = module.relations
    while (oldSize != size) {
      oldSize = size
      result = super.visit(module)
    }
    result

  override def visitRelation(relation: Relation): Seq[Relation] =
    this.currentRelation = relation
    super.visitRelation(relation)

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match {
      case Demand(ts) =>
        for (t <- ts; v <- t.vars)
          if (v.typeIs(_.mode == Mode.Binding))
            addCurrentDemand(v)
      case Call(name, ts) =>
        val demand = demandOf(name)
        val ixs = demandPositionsOf(name)
        for (ix <- ixs; v <- ts(ix).vars)
          if (v.typeIs(_.mode == Mode.Binding))
            addCurrentDemand(v)
      case NegCall(name, ts) =>
        val demand = demandOf(name)
        val ixs = demandPositionsOf(name)
        for (ix <- ixs; v <- ts(ix).vars)
          if (v.typeIs(_.mode == Mode.Binding))
            addCurrentDemand(v)
      case _ => // nothing
    }
    super.visitAtom(atom)
