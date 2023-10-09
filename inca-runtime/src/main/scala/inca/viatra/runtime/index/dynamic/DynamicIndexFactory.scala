package inca.viatra.runtime.index.dynamic

import inca.viatra.runtime.db.Database

trait DynamicIndexFactory {
  def makeIndex(database: Database): DynamicIndex
}
