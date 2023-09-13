package inca.util.measurement

import inca.backend.ir.Datalog


object MetricUtils {
  def statistics(module: Datalog.Module): Map[String, Int] = Seq(
      "relations" -> module.pats.size,
      "rules" -> module.pats.map(p => p.bodies.size).sum,
      "input_relations" -> module.pats.count(_.name.startsWith("input$")),
      "atoms" -> module.pats.map(_.bodies.map(_.atoms.size).sum).sum,
      "dispatch_relations" -> module.pats.count(_.name.startsWith("dispatch$")),
    ).toMap

  def printStatistics(module: Datalog.Module): String =
    statistics(module).map(kv => s"${kv._1}: ${kv._2}").mkString(", ")
}
