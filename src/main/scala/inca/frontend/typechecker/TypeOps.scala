package inca.frontend.typechecker

import inca.frontend.core.Core._
import inca.runtime.context.LanguageMetaInfo
import truechange.{AnyType, JavaLitType, ListType, SortType}

object TypeOps {

  def subtype(ty1: TypeAnno, ty2: TypeAnno, languageMetaInfo: LanguageMetaInfo): Boolean =
    meet(ty1, ty2, languageMetaInfo) == ty1

  def meet(ty1: TypeAnno, ty2: TypeAnno, languageMetaInfo: LanguageMetaInfo): TypeAnno = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TAny, _) => ty2
    case (_, TAny) => ty1
//    case (TUnbounded(wrappedTy), _) if wrappedTy == ty2 => Some(wrappedTy)
//    case (_, TUnbounded(wrappedTy)) if ty1 == wrappedTy => Some(wrappedTy)
    case (TAnyLinked, _:TLinked) => ty2
    case (_:TLinked,TAnyLinked) => ty1
    case (TNode(name1), TNode(name2)) =>
      if (languageMetaInfo.nodeSupertypes.containsEntry(SortType(name1) -> SortType(name2)))
        ty1
      else if (languageMetaInfo.nodeSupertypes.containsEntry(SortType(name2) -> SortType(name1)))
        ty2
      else
        TNothing
    case (TList(s1), TList(s2)) => TList(meet(s1, s2, languageMetaInfo).asInstanceOf[TLinked])
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size => TTuple(tys1.zip(tys2).map(tt => meet(tt._1, tt._2, languageMetaInfo)))
    case _ => TNothing
  }


  def truechangeTypeToTypeAnno(ty: truechange.Type): TypeAnno = ty match {
    case AnyType => TAny
    case SortType(name) => TNode(name)
    case ListType(ty) =>
      val convertedTy = truechangeTypeToTypeAnno(ty)
      convertedTy match {
        case linked: TLinked => TList(linked)
        case _ => throw new IllegalArgumentException()
      }
    case _ => throw new UnsupportedOperationException(s"conversion of $ty from truechange to inca not supported")
  }

  def truechangeLitTypeToTypeAnno(ty: truechange.LitType): TypeAnno = ty match {
      // TODO distinguish between cl
    case JavaLitType(cl) => throw new IllegalArgumentException()
    case _ => throw new IllegalArgumentException("")
  }
}
