package inca.frontend.typechecker

import inca.frontend.core._
import inca.runtime.context.LanguageMetaInfo

sealed trait StmType {
  def asType: Type = this match {
    case NoYield => TUnit
    case Yields(ty) => ty
  }

  override def toString: String = asType.prettyprint

//  def meet(other: StmType, lang: LanguageMetaInfo): StmType = (this, other) match {
//    case (NoYield, _) => NoYield
//    case (_, NoYield) => NoYield
//    case (Yields(ty1), Yields(ty2)) => Yields(meet(ty1, ty2, lang))
//  }

}
case object NoYield extends StmType
case class Yields(ty: Type) extends StmType

