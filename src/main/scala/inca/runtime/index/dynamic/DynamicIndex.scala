package inca.runtime.index.dynamic

import inca.runtime.Database
import inca.runtime.index.Index
import truechange.Edit

trait DynamicIndex extends Index {
  protected var database: Database = _
  def setDatabase(database: Database): Unit = this.database = database

  /** process change and updates index/notifies listeners accordingly */
  def processEdit(edit: Edit): Unit
}
