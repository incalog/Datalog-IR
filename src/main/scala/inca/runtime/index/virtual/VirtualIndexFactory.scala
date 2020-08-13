package inca.runtime.index.virtual

import inca.runtime.Database
import inca.runtime.index.VirtualKey

/** Virtual indices can be created on demand since they do not cache data themselves. */
trait VirtualIndexFactory {
  def makeIndex(key: VirtualKey, database: Database): VirtualIndex
}
