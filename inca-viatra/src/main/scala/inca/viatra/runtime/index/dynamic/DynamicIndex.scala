package inca.viatra.runtime.index.dynamic

import inca.viatra.runtime.db.Database
import inca.viatra.runtime.index.Index
import truechange.CoreEdit
import scala.compiletime.uninitialized

/** A dynamic index implements its own edit processing to update the index. */
trait DynamicIndex extends Index {
  protected var database: Database = uninitialized

  def setDatabase(database: Database): Unit = this.database = database

  def startProcessEditScript(): Unit

  def endProcessEditScript(): Unit

  /** processes edit to update this index accordingly */
  def processEdit(edit: CoreEdit): Unit
}
