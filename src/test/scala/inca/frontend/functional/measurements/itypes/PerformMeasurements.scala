package inca.frontend.functional.measurements.itypes

object PerformMeasurements extends scala.App {
  val configs = MeasurementConfig.generate(200, 50)
  // TODO how will the type checker for LetStar look like (needs support for lists or should we use ADT to encode lists?)?
}