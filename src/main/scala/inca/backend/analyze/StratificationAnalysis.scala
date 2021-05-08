package inca.backend.analyze

import inca.backend.ir.Datalog._

trait AnalysisException extends Exception

case class StratificationExpection(msg: String) extends Exception

object StratificationAnalysis {
  def analyze(mod: Module): Unit = {
    if (hasNegCycle(mod))
      throw StratificationExpection("Datalog program contains cyclic dependency with a negation")
  }

  def hasNegCycle(mod: Module): Boolean = {
    val graph = ConstructDependencyGraph(mod)
    val cycles = graph.cycles()
    cycles.exists { cycle =>
      val edges = cycle.flatMap { n => graph.edges(n) }
      edges.exists(_._2)
    }
  }
}
