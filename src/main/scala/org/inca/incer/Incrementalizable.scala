package org.inca.incer

import org.inca.incer.indices.Indices

trait Incrementalizable {

  def insert(indices : Indices) : Unit = {  }
  def delete(indices : Indices) : Unit = {  }

}
