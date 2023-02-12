package inca.backend.optimize

import inca.runtime.context.DataModel

trait Optimization {
  def optimizer(dataModel: DataModel): Optimizer
}
