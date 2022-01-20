package inca.backend.ir

import inca.backend.ir.Datalog._
import inca.runtime.context.DataModel
import inca.util.{Scala, ScalaTyper}
import truechange.SortType

trait TypeOps extends ScalaTyper {

  def subtype(ty1: Type, ty2: Type, dataModel: DataModel): Boolean =
    meet(ty1, ty2, dataModel).contains(ty1)

  def meet(ty1: Type, ty2: Type, dataModel: DataModel): Option[Type] = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => Some(ty1)
    case (TAny, _) => Some(ty2)
    case (_, TAny) => Some(ty1)
    case (TAnyLinked, _:TLinked) => Some(ty2)
    case (_:TLinked,TAnyLinked) => Some(ty1)
    case (TNode(name1), TNode(name2)) =>
      if (dataModel.nodeSupertypes.containsEntry(SortType(name1) -> SortType(name2)))
        Some(ty1)
      else if (dataModel.nodeSupertypes.containsEntry(SortType(name2) -> SortType(name1)))
        Some(ty2)
      else
        None
    case (TData(name1), TData(name2)) =>
      if (dataModel.nodeSupertypes.containsEntry(SortType(name1) -> SortType(name2)))
        Some(ty1)
      else if (dataModel.nodeSupertypes.containsEntry(SortType(name2) -> SortType(name1)))
        Some(ty2)
      else
        None
    case (TList(s1), TList(s2)) => meet(s1, s2, dataModel).map(t => TList(t.asInstanceOf[TLinked]))
    case (TScala(s1), TScala(s2)) =>
      if (subtypeScala(s1.tree, s2.tree))
        Some(ty1)
      else if (subtypeScala(s2.tree, s1.tree))
        Some(ty2)
      else
        None
    case (_, TScala(_)) => meet(TScala(Scala(ty1.asScala)), ty2, dataModel)
    case (TScala(_), _) => meet(ty1, TScala(Scala(ty2.asScala)), dataModel)
    case _ => None
  }

  def meet(tys: Iterable[Type], languageMetaInfo: DataModel): Option[Type] = {
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
