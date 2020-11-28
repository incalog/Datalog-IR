package inca.frontend.extensions.switch_

import inca.frontend.core

trait Syntax extends core.Syntax {
  def Switch(bodies: Seq[Body]): Statement
}
