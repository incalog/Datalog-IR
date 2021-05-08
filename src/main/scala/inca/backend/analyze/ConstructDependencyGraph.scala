package inca.backend.analyze

import inca.backend.ir.Datalog._
import inca.backend.analyze.DependencyGraph

object ConstructDependencyGraph {

  def apply(module: Module): DependencyGraph = {
    val pats = collectPatterns(module)

    val graph = new DependencyGraph()
    module.pats.foreach { pat =>
      pat.bodies.foreach { body =>
        body.atoms.foreach {
          case Call(name, _, _, neg) =>
            graph.addEdge(pat, pats(name), neg)
          case _ => // do nothing
        }
      }
    }
    graph
  }
  private def collectPatterns(module: Module): Map[String, Pattern] =
    module.pats.map { pat =>
      pat.name -> pat
    }.toMap
}
