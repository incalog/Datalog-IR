package inca.ir.typing

import inca.ir.extension.data.{CaseDefinition, DataDefinition}
import inca.ir.{ModuleEntry, Relation}
import inca.ir.extension.demand
import inca.util.Graph

enum DependencyInfo:
  case PositiveCall
  case NegativeCall
  case AggregationCall
  case TypeReference

class DependencyGraph extends Graph[ModuleEntry, DependencyInfo]:
  def negativeCycles: List[List[(ModuleEntry, DependencyInfo)]] =
    cyclesWithInfo.filter { cycle =>
      cycle.count(_._2 == DependencyInfo.NegativeCall) % 2 != 0
    }

  override protected def nodeToGraphViz(n: ModuleEntry): String =
    namify(n.name.name)

  override protected def nodeGraphVizAttributes(from: ModuleEntry): String = from match
    case r: Relation if demand.isDemandRelation(r) =>
      "fillcolor=darkorange3, style=filled"
    case _: (DataDefinition | CaseDefinition) =>
      "fillcolor=green2, style=filled"
    case _ =>
      "fillcolor=black, style=filled, fontcolor=white"

  override protected def edgeGraphVizAttributes(from: ModuleEntry, to: ModuleEntry, info: DependencyInfo): String =
    val ix = outermostCycles.indexWhere(l => l.contains(from) && l.contains(to))
    if (ix >= 0)
      s"color=${cycleColor(ix)}"
    else info match
      case DependencyInfo.NegativeCall => "color=red"
      case DependencyInfo.AggregationCall => "color=brown"
      case DependencyInfo.TypeReference => "color=darkorange3"
      case _ => "color=black"

  private def cycleColor(i: Int): String = i match {
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

