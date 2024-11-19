package inca.util

import inca.ir.extension.aggregate.Aggregate
import inca.ir.{Call, Module, Name, RefByName, Relation}
import inca.util.DependencyGraph.{AggregationCall, DependencyEdge, NegativeCall, PositiveCall}
import inca.util.datastructures.Graph


object DependencyGraph:
  sealed trait DependencyEdge

  case object PositiveCall extends DependencyEdge

  case object NegativeCall extends DependencyEdge

  case object AggregationCall extends DependencyEdge


class DependencyGraph(module: Module) extends Graph[String, DependencyEdge]:
  val rels: Map[String, Relation] = module.relations
  module.relations.foreach { (relName, r) =>
    this.addNode(relName)
    r.bodies.foreach { body =>
      body.atoms.foreach {
        case Call(RefByName(name), args, false) =>
          this.addEdge(relName, name.name, PositiveCall)
        case Call(RefByName(name), args, true) =>
          this.addEdge(relName, name.name, NegativeCall)
        case agg@Aggregate(rel: Name, _, _) =>
          this.addEdge(relName, s"$rel$$${agg.hashCode()}", AggregationCall)
        case _ => // do nothing
      }
    }
  }

  override def cloneGraph(): Graph[String, DependencyEdge] =
    val g = new DependencyGraph(module)
    g.nodes ++= this.nodes
    g.edges ++= this.edges
    g

  override protected def nodeToGraphViz(n: String): String =
    n.replace("$", "_")

  override protected def nodeGraphVizAttributes(n: String): String =
    "fillcolor=black, style=filled, fontcolor=white"

  def cycleColor(i: Int): String = i match {
    case 0 => "aquamarine3"
    case 1 => "darkorchid"
    case 2 => "dodgerblue3"
    case 3 => "gold4"
    case 4 => "maroon4"
    case 5 => "palegreen4"
    case 6 => "sienna3"
    case 7 => "tomato3"
    case _ => "black"
  }

  override protected def edgeGraphVizAttributes(from: String, to: String, kind: DependencyEdge): String =
    val ix = outermostCycles.indexWhere(l => l.contains(from) && l.contains(to))
    if (ix >= 0)
      s"color=${cycleColor(ix)}"
    else kind match {
      case NegativeCall => "color=red"
      case _ => "color=black"
    }
