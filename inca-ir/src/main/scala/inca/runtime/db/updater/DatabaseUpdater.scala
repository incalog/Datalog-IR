package inca.runtime.db.updater

import inca.runtime.db.Database
import truechange.*

trait DatabaseUpdater {
  def db: Database

  def startProcessEditScript(): Unit
  def endProcessEditScript(): Unit
  def processEdit(edit: CoreEdit): Unit

  protected def editError(msg: String) = throw new IllegalStateException("Processing edit script failed: " + msg)
}

