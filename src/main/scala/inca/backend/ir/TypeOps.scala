package inca.backend.ir

import inca.backend.ir.GP._
import inca.runtime.context.LanguageMetaInfo
import truechange.SortType

object TypeOps {

  def meet(ty1: TypeAnno, ty2: TypeAnno, languageMetaInfo: LanguageMetaInfo): Option[TypeAnno] = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => Some(ty1)
    case (TAnyLinked, _:TLinked) => Some(ty2)
    case (_:TLinked,TAnyLinked) => Some(ty1)
    case (TNode(name1), TNode(name2)) =>
      if (languageMetaInfo.nodeSubtypes.containsEntry(SortType(name1) -> SortType(name2)))
        Some(ty1)
      else if (languageMetaInfo.nodeSubtypes.containsEntry(SortType(name2) -> SortType(name1)))
        Some(ty2)
      else
        None
    case (TList(s1), TList(s2)) => meet(s1, s2, languageMetaInfo).map(t => TList(t.asInstanceOf[TLinked]))
    case _ => None
  }

  def meet(tys: Iterable[TypeAnno], languageMetaInfo: LanguageMetaInfo): Option[TypeAnno] = {
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
