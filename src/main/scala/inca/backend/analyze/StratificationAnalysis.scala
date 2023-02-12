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
    val cycles = graph.cycles
    cycles.filter { cycle =>
      val edges = cycle.flatMap { n => graph.edges(n) }
      val edgesOfCycle = edges.filter { e => cycle.contains(e._1) }
      edgesOfCycle.exists(_._2 == NegativeCall)
    }
  }
}
