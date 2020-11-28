package inca.frontend.extensions.foreach

import inca.frontend.core

trait Syntax extends core.Syntax {
  def Foreach(name: Name, exp: Expression, body: Body): Statement
}
