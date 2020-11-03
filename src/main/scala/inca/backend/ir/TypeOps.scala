package inca.backend.ir

import inca.backend.ir.GP._
import inca.runtime.context.LanguageMetaInfo
import truechange.SortType

object TypeOps {

  def subtype(ty1: Type, ty2: Type, languageMetaInfo: LanguageMetaInfo): Boolean =
    meet(ty1, ty2, languageMetaInfo).contains(ty1)

  def meet(ty1: Type, ty2: Type, languageMetaInfo: LanguageMetaInfo): Option[Type] = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => Some(ty1)
    case (TAny, _) => Some(ty2)
    case (_, TAny) => Some(ty1)
    case (TUnbounded(wrappedTy), _) if wrappedTy == ty2 => Some(wrappedTy)
    case (_, TUnbounded(wrappedTy)) if ty1 == wrappedTy => Some(wrappedTy)
    case (TAnyLinked, _:TLinked) => Some(ty2)
    case (_:TLinked,TAnyLinked) => Some(ty1)
    case (TNode(name1), TNode(name2)) =>
      if (languageMetaInfo.nodeSupertypes.containsEntry(SortType(name1) -> SortType(name2)))
        Some(ty1)
      else if (languageMetaInfo.nodeSupertypes.containsEntry(SortType(name2) -> SortType(name1)))
        Some(ty2)
      else
        None
    case (TList(s1), TList(s2)) => meet(s1, s2, languageMetaInfo).map(t => TList(t.asInstanceOf[TLinked]))
    case _ => None
  }

  def meet(tys: Iterable[Type], languageMetaInfo: LanguageMetaInfo): Option[Type] = {
    if (tys.isEmpty)
      None
    else {
      var ty = tys.head
      for (other <- tys.tail) {
        ty = meet(ty, other, languageMetaInfo).getOrElse(return None)
      }
      Some(ty)
    }
  }
}
