package inca.frontend.constraint.typechecker

import inca.frontend.constraint.core._

sealed trait StmType {
  def asType: Type = this match {
    case NoYield => TUnit
    case Yields(ty) => ty
  }

  override def toString: String = asType.prettyprint
}
case object NoYield extends StmType
case class Yields(ty: Type) extends StmType
