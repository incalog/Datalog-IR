package inca.viatra.runtime.index.virtual

import inca.viatra.runtime.db.Database
import inca.viatra.runtime.index.VirtualKey

/** Virtual indices can be created on demand since they do not cache data themselves. */
trait VirtualIndexFactory {
  def makeIndex(key: VirtualKey, database: Database): VirtualIndex
}
