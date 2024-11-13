package inca.viatra.compile

import inca.foreign.scala.ir.primitive
import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.Aggregate
import inca.ir.{Atom, Body, Call, Name, Param, RefByName, Relation, Var}
import inca.ir.visitors.IRVisitor
import inca.util.{DependencyGraph, Gensym}
import inca.ir.extension.edbdata


class SubstituteCallsRewriter(find: Name, replace: Name) extends IRVisitor with primitive.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Call(RefByName(name), args, neg) if name == find =>
      Seq(Call(RefByName(replace), args, neg))
    case Aggregate(RefByName(name), args, op) if name == find =>
      Seq(Aggregate(RefByName(replace), args, op))
    case _ => super.visitAtom(atom)

/**
 * This rewriting works based on a couple of assumptions:
 * 1. We assume each aggregation in a relation `R` encapsulates the input in a separate collecting relation `Q`.
 * 2. The signature of the collecting relation `Q` and `R` must be the same.
 *
 * Based on these assumptions we perform the following steps:
 * 1. Guarantee that each relation contains at most one aggregation.
 * 2. If a relation `R` contains an aggregation over a relation `Q` in the same strongly connected component (scc):
 * 2.1 Rename `R` by appending a suffix "Wrapped"
 * 2.2 Redirect all calls to `R` in the scc to `R$Wrapped`
 * 2.3 Introduce a new relation `R` that queries and aggregates over `R$Wrapped`
 */
class TimelyLatticeAggregationRewriter extends edbdata.Visitor with IRVisitor with primitive.Visitor:
  val gensym: Gensym = Gensym()

  var scc: Seq[Seq[String]] = Seq()
  var relations: Map[String, Relation] = Map()

  override def visitModule(module: ir.Module): ir.Module = preserveHints(module) {
    module.contents.foreach(c => gensym.register(c.name.name))
    scc = DependencyGraph(module).cycles
    relations = module.relations
    val remainingContent = module.contents.filter {
      case Relation(name, params, bodies) => false
      case _ => true
    }
    val ir.Module(name, lang, contents) = super.visitModule(module)
    ir.Module(name, lang, remainingContent ++ relations.values)
  }

  var currentRelation: Relation = null
  var numberOfAggregations: Int = 0

  override def visitRelation(relation: Relation): Seq[Relation] =
    currentRelation = relation
    numberOfAggregations = 0
    super.visitRelation(relation)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case agg: Aggregate if agg.rel.name == currentRelation.name =>
      throw IllegalStateException("Can not handle directly recursive aggregation. Please wrap the input of the aggregation in a separate relation.")
    case agg: Aggregate if numberOfAggregations > 1 =>
      throw IllegalStateException("At most one aggregation over lattice values can occur in a pattern!")
    case agg: Aggregate =>
      numberOfAggregations += 1
      val allSCCs = scc.filter(_.contains(currentRelation.name.name))
        .filter(_.contains(agg.rel.name))
      // only if we aggregate over a relation in the same strongly connected component
      allSCCs.foreach { currentScc =>
        createDoubleAggregation(currentScc, currentRelation, agg)
      }
      super.visitAtom(atom)
    case _ => super.visitAtom(atom)

  private def createDoubleAggregation(currentSCC: Seq[String], rel: Relation, agg: Aggregate): Unit = gensym.scoped {
    val qualifiedName = gensym.fresh(s"${rel.name}$$Wrapped")

    // the wrapped relation just does whatever the original relation was doing
    relations += qualifiedName -> Relation(Name(qualifiedName), rel.params, rel.bodies)

    // redirect all calls in the scc to the wrapper function
    (currentSCC :+ qualifiedName).filter(n => n != rel.name.name).foreach { sccRelName =>
      val relation = relations(sccRelName)
      val rewriter = new SubstituteCallsRewriter(rel.name, Name(qualifiedName))
      val Seq(wrappedRel) = rewriter.visitRelation(relation)
      relations += sccRelName -> wrappedRel
    }

    // FIXME: This assumes that agg.rel and rel have the same signature.
    //  Otherwise we don't know over which column we need to aggregate.
    //  We could work around this, by precisely tracking the dataflow.
    val aggRelation = relations(agg.rel.name.name)
    aggRelation.params.zipAll(rel.params, null, null).foreach {
      case (Param(name1, ty1), Param(name2, ty2)) if name1 == name2 && ty1 == ty2 => // nothing
      case _ => throw IllegalStateException("Ambiguous aggregation rewrite!")
    }

    // rewrite the original relation to aggregate over the wrapper relation
    rel.params.foreach(p => gensym.register(p.name.name))
    val Seq(aggregatedColumn) = agg.aggregationColumns
    val aggParam = rel.params(aggregatedColumn)
    val wildcardParam = Param(Name(gensym.fresh("dummy")), aggParam.ty)
    val callParams = rel.params.updated(aggregatedColumn, wildcardParam)

    val orgRelation = Relation(rel.name, rel.params, Seq(
      Body(Seq(
        Call(Name(qualifiedName), callParams.map(p => Var(p.name).arg)),
        Aggregate(RefByName(Name(qualifiedName)), agg.args, agg.op)
      ))
    ))

    relations += (rel.name.name -> orgRelation)
  }