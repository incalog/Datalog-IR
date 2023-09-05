package inca.frontend.functional.itypes

import inca.measurements.util.Config

case class MeasurementConfig(
    depth: Int,
    gen: GenerateProg,
    edit: EditScenario,
    warmup: Int,
    runs: Int)
    extends Config {
  def name: String = ""
}

object MeasurementConfig {
  def generate(
      depth: Int,
      warmup: Int,
      runs: Int
    ): Seq[MeasurementConfig] = {
    val gens = Seq(GenerateStarDependencyProg, GenerateChainDependencyProg)
    val edits = Seq(
      NumEditScenario,
      RefEditScenario,
      ParamEditScenario,
      AnnoEditScenario,
      LambdaEditScenario,
      AddAppEditScenario
    )
    for {
      gen <- gens
      edit <- edits
    } yield MeasurementConfig(depth, gen, edit, warmup, runs)
  }
}
