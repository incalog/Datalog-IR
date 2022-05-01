package inca.backend.analyze

import inca.backend.analyze.DependencyGraph.NegativeCall
import inca.backend.ir.Datalog._

trait AnalysisException extends Exception

case class StratificationException(msg: String) extends Exception

object StratificationAnalysis {
  def analyze(mod: Module): Unit = {
    val negCycles = detNegCycles(mod)
    if (negCycles.nonEmpty) {
      val prettyNegCycles = negCycles.map{ c => c.mkString(" -> ")}.mkString("{", "}, {", "}")
      throw StratificationException(s"Datalog program contains cyclic dependency with a negation: $prettyNegCycles")
    }
  }

  def detNegCycles(mod: Module): Seq[Seq[Name]] = {
    val graph = new DependencyGraph(mod)
    // withDemand=false: check for stratification of original program only,
    // negative cycles introduced by demand transformation are handled separately
    val scc = graph.stronglyConnectedComponents(false)

    scc.filter{ comp =>
      val edges = comp.flatMap{ n => graph.edges.getOrElse(n, List()) }
      val edgesOfComp = edges.filter { e => comp.contains(e._1) }
      edgesOfComp.exists {
        case (name, NegativeCall) => true
        case _ => false
      }
    }
  }
}
