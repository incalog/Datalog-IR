package inca.runtime.index.dynamic

import inca.runtime.db.Database
import inca.runtime.index.Index
import truechange.CoreEdit

/** A dynamic index implements its own edit processing to update the index. */
trait DynamicIndex extends Index {
  protected var database: Database = _
  def setDatabase(database: Database): Unit = this.database = database

  def startProcessEditScript(): Unit
  def endProcessEditScript(): Unit

  /** processes edit to update this index accordingly */
  def processEdit(edit: CoreEdit): Unit
}
