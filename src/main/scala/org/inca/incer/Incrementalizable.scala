package org.inca.incer

import org.inca.incer.indices.IncARuntimeContext

trait Incrementalizable {

  def insert(indices: IncARuntimeContext): Unit = {}

  def insert(indices: IncARuntimeContext, recursive: Boolean): Unit = {}

  def delete(indices: IncARuntimeContext): Unit = {}

  def delete(indices: IncARuntimeContext, recursive: Boolean): Unit = {}
}
