package inca.frontend_old.extensions.foreach

import inca.frontend_old.core

trait Syntax extends core.Syntax {
  def Foreach(name: Name, exp: Expression, body: Body): Statement
}
