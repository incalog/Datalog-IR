package inca.frontend_old.extensions.switch_

import inca.frontend_old.core

trait Syntax extends core.Syntax {
  def Switch(bodies: Seq[Body]): Statement
}
