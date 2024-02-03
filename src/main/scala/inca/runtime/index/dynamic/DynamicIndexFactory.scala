package inca.runtime.index.dynamic

import inca.runtime.db.Database

trait DynamicIndexFactory {
  def makeIndex(database: Database): DynamicIndex
}
