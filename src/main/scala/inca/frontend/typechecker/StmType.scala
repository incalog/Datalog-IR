package inca.frontend.typechecker

import inca.frontend.core._
import inca.runtime.context.LanguageMetaInfo

sealed trait StmType {
  def asType: Type = this match {
    case NoTerminator => TUnit
    case Terminator(ty) => ty
  }

  def meet(other: StmType, lang: LanguageMetaInfo): StmType = (this, other) match {
    case (NoTerminator, _) => NoTerminator
    case (_, NoTerminator) => NoTerminator
    case (Terminator(ty1), Terminator(ty2)) => Terminator(TypeOps.meet(ty1, ty2, lang))
  }
}
case object NoTerminator extends StmType
case class Terminator(ty: Type) extends StmType

