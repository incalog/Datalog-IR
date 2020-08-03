package inca.runtime.index.dynamic

import inca.runtime.Database
import inca.runtime.index.Index
import truechange.Edit

/** A dynamic index implements its own edit processing to update the index. */
trait DynamicIndex extends Index {
  protected var database: Database = _
  def setDatabase(database: Database): Unit = this.database = database

  /** processes edit to update this index accord */
  def processEdit(edit: Edit): Unit
}
