package inca.backend.optimize

object Optimization:
  case object BodyMustFail extends Exception

trait Optimization:
  def optimizer(): Optimizer
  def name: String
