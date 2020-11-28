package inca.frontend.extensions.forallExists

import inca.frontend.core

trait Syntax extends core.Syntax {
  def Forall(name: Name, exp: Expression, body: Body): Statement
  def Exists(name: Name, exp: Expression, body: Body): Statement
}
