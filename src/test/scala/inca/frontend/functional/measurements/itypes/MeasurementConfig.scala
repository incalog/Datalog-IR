package inca.frontend.functional.measurements.itypes

case class MeasurementConfig(depth: Int, gen: GenerateProg, edit: EditScenario, warmupMeasurements: Int, numMeasurements: Int)

object MeasurementConfig {
  def generate(depth: Int, warmupMeasurements: Int, numMeasurements: Int): Seq[MeasurementConfig] = {
    val gens = Seq(GenerateStarDependencyProg, GenerateChainDependencyProg)
    val edits = Seq(NumEditScenario, RefEditScenario, ParamEditScenario, AnnoEditScenario, LambdaEditScenario, AddAppEditScenario)
    for (gen <- gens;edit <- edits) yield MeasurementConfig(depth, gen, edit, warmupMeasurements, numMeasurements)
  }
}
