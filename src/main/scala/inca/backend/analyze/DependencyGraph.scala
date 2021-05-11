package inca.backend.analyze

import inca.backend.hints.DataHints.{ConstructorKey, DataTypeKey, SelectorKey}
import inca.backend.ir.Datalog.{Call, Module, Name, Pattern}
import inca.backend.transform.magic.demand.DemandTransformation.demandPatternPrefix


class DependencyGraph(module: Module) extends Graph[Pattern, Boolean] {
  val pats: Map[Name, Pattern] = module.pats.map(p => p.name -> p).toMap
  module.pats.foreach { pat =>
    pat.bodies.foreach { body =>
      body.atoms.foreach {
        case Call(name, _, _, neg) =>
          this.addEdge(pat, pats(name), neg)
        case _ => // do nothing
      }
    }
  }

  override protected def nodeToGraphViz(n: Pattern): String = n.name.replace("$", "_")

  def isDataNode(p: Pattern): Boolean = p.hasHint(DataTypeKey) || p.hasHint(ConstructorKey) || p.hasHint(SelectorKey)

  override protected def nodeGraphVizAttributes(from: Pattern): String =
    if (isDataNode(from))
      "fillcolor=green2, style=filled"
    else if (from.name.startsWith(demandPatternPrefix))
      "fillcolor=darkorange3, style=filled"
    else
      "fillcolor=black, style=filled, fontcolor=white"

  override protected def edgeGraphVizAttributes(from: Pattern, to: Pattern, isDeletion: Boolean): String = {
    if (isDeletion)
      "color=red"
//    else if (isDataNode(from) && isDataNode(to))
//      "color=green2"
//    else if (from.name.startsWith(demandPatternPrefix) && to.name.startsWith(demandPatternPrefix))
//      "color=darkorange3"
    else
      "color=black"
  }
}