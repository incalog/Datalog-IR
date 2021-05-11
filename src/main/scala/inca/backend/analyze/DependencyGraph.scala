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

  override protected def edgeToGraphViz(from: Pattern, to: Pattern, info: Boolean): String = if (info) "color=red" else "color=black"

  override protected def nodeToGraphViz(n: Pattern): String = n.name.replace("$", "_")

  override protected def nodeGraphVizAttributes(from: Pattern): String =
    if (from.hasHint(DataTypeKey) || from.hasHint(ConstructorKey) || from.hasHint(SelectorKey))
      "fillcolor=green2, style=filled"
    else if (from.name.startsWith(demandPatternPrefix))
      "fillcolor=darkorange3, style=filled"
    else
      "fillcolor=black, style=filled, fontcolor=white"
}