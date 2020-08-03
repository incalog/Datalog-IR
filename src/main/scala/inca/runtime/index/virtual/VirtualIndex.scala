package inca.runtime.index.virtual

import inca.runtime.Database
import inca.runtime.index.Index

/** A virtual index stores no data of its own but uses other indices to answer queries */
trait VirtualIndex extends Index {
  protected var database: Database = _
  def setDatabase(database: Database): Unit = this.database = database
}
