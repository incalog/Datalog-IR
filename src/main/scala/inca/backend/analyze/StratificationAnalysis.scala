package inca.backend.analyze

import inca.backend.ir.Datalog._

trait AnalysisException extends Exception

case class StratificationExpection(msg: String) extends Exception

object StratificationAnalysis {
  def analyze(mod: Module): Unit = {
    val negCycles = detNegCycles(mod)
    if (negCycles.nonEmpty) {
      val prettyNegCycles = negCycles.map{ c => c.map{ n => n.name }.mkString(" -> ")}.mkString("{", "}, {", "}")
      throw StratificationExpection(s"Datalog program contains cyclic dependency with a negation: $prettyNegCycles")
    }
  }

  def detNegCycles(mod: Module): Seq[Seq[Pattern]] = {
    val graph = new DependencyGraph(mod)
    val cycles = graph.cycles()
    cycles.filter { cycle =>
      val edges = cycle.flatMap { n => graph.edges(n) }
      val edgesOfCycle = edges.filter { e => cycle.contains(e._1) }
      edgesOfCycle.exists(_._2)
    }
  }
}
