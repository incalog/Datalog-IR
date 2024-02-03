package inca.backend.transform

import inca.runtime.context.DataModel

trait Transformation {
  def transformer(dataModel: DataModel): Transformer
}
