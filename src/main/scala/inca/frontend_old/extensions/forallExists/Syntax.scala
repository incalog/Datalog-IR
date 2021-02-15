package inca.frontend_old.extensions.forallExists

import inca.frontend_old.core

trait Syntax extends core.Syntax {
  def Forall(name: Name, exp: Expression, body: Body): Statement
  def Exists(name: Name, exp: Expression, body: Body): Statement
}
