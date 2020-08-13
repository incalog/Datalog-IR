package inca.runtime.index.dynamic

import inca.runtime.Database

trait DynamicIndexFactory {
  def makeIndex(database: Database): DynamicIndex
}
