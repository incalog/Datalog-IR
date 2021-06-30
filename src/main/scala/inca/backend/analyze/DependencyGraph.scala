package inca.backend.analyze

import inca.backend.analyze.DependencyGraph._
import inca.backend.hints.DataHints.{ConstructorKey, DataTypeKey, SelectorKey}
import inca.backend.ir.IR.{Call, Computed, CountAggregation, CustomAggregation, Module, Name, Pattern}
import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix

object DependencyGraph {
  sealed trait DependencyEdge
  case object PositiveCall extends DependencyEdge
  case object NegativeCall extends DependencyEdge
  case object CountAggregationCall extends DependencyEdge
  case object CustomAggregationCall extends DependencyEdge
}

class DependencyGraph(module: Module) extends Graph[Name, DependencyEdge] {
  val pats: Map[Name, Pattern] = module.pats.map(p => p.name -> p).toMap
  module.pats.foreach { pat =>
    this.addNode(pat.name)
    pat.bodies.foreach { body =>
      body.atoms.foreach {
        case Call(name, _, _, neg) => this.addEdge(pat.name, name, if (neg) NegativeCall else PositiveCall)
        case Computed(_, CountAggregation(name, _)) => this.addEdge(pat.name, name, CountAggregationCall)
        case Computed(_, CustomAggregation(_, _, _, name, _, _)) => this.addEdge(pat.name, name, CustomAggregationCall)
        case _ => // do nothing
      }
    }
  }

  override protected def nodeToGraphViz(n: Name): String = n.replace("$", "_")

  def isDataNode(p: Pattern): Boolean = p.hasHint(DataTypeKey) || p.hasHint(ConstructorKey) || p.hasHint(SelectorKey)

  override protected def nodeGraphVizAttributes(n: Name): String = {
    val node = pats(n)
    if (isDataNode(node))
      "fillcolor=green2, style=filled"
    else if (n.startsWith(demandPatternPrefix))
      "fillcolor=darkorange3, style=filled"
    else
      "fillcolor=black, style=filled, fontcolor=white"
  }

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

  override protected def edgeGraphVizAttributes(from: Name, to: Name, kind: DependencyEdge): String = {
    val ix = outermostCycles.indexWhere(l => l.contains(from) && l.contains(to))
    if (ix >= 0)
      s"color=${cycleColor(ix)}"
    else kind match {
      case NegativeCall => "color=red"
      case _ => "color=black"
    }
  }
}