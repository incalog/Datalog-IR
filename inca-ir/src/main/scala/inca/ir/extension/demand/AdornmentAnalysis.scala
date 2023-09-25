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

  protected var currentRelations: Map[String, Relation] = Map()
  protected var currentRelation: Relation = _

  def addCurrentDemand(v: Var): Unit =
    if (currentRelation.params.map(_.name.name).contains(v.name.name)) {
      demandedParams += currentRelation.name.name -> v.name.name
    } else {
      throw new IllegalArgumentException(s"Demanded variable $v is not a parameter of $currentRelation")
    }

  def currentDemand: collection.Set[String] =
    demandOf(currentRelation.name)
  def demandOf(rel: Name): collection.Set[String] =
    demandedParams.get(rel.name)
  def demandedPositionsOf(rel: Name): Seq[Int] =
    val ps = demandedParams.get(rel.name)
    currentRelations.get(rel.name) match
      case Some(r) =>
        r.params.zipWithIndex.flatMap((p,ix) =>
          if (ps.contains(p.name.name))
            Some(ix)
          else
            None
        )
      case None =>
        Seq.empty
  def demandedArgsOf(rel: Name, args: Seq[Term]): Seq[Term] =
    val ixs = demandedPositionsOf(rel)
    for (ix <- ixs.toSeq.sorted) yield
      args(ix)
  def demandParamsOf(rel: Name): Seq[Param] =
    val ps = demandedParams.get(rel.name)
    val r = currentRelations(rel.name)
    r.params.flatMap { p =>
      if (ps.contains(p.name.name))
        Some(p)
      else
        None
    }

  def analyzeModule(module: Module): Module =
    var oldSize = -1
    var result = module
    currentRelations = module.relations
    while (true) {
      val newSize = demandedParams.size
      if (oldSize == newSize)
        return result
      oldSize = newSize
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
        val demandedArgs = demandedArgsOf(name, ts)
        for (case v: Var <- demandedArgs)
          if (v.typeIs(_.mode == Mode.Binding))
            addCurrentDemand(v)
      case NegCall(name, ts) =>
        // relevant in case the program contains double negation as in Not(NegCall(P, x)), which equivalent to Call(P, x)
        val demandedArgs = demandedArgsOf(name, ts)
        for (case v: Var <- demandedArgs)
          if (v.typeIs(_.mode == Mode.Binding))
            addCurrentDemand(v)
      case _ => // nothing
    }
    super.visitAtom(atom)
