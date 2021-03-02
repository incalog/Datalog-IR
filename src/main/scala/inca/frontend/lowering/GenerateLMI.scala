package inca.frontend.lowering

import inca.frontend.core._
import inca.runtime.context.LanguageMetaInfo
import truechange.{JavaLitType, SortType}

import scala.collection.immutable.MultiDict

class GenerateLMI(module: Module) {

  def transModule(): LanguageMetaInfo = {
    val datas = module.content.collect { case data: DataDef => data }

    val subtyps = for (DataDef(_, _, name, constrs) <- datas;
                       DataConstructor(cname, _) <- constrs)
      yield SortType(cname.name) -> SortType(name.name)
    val kidLinks = for (DataDef(_, _, _, constrs) <- datas;
                        DataConstructor(cname, paramTypes) <- constrs;
                        (ty,ix) <- paramTypes.zipWithIndex if ty.isInstanceOf[TData])
      yield (cname.name, "_" + ix) -> SortType(ty.asInstanceOf[TData].name.name)
    val litLinks = for (DataDef(_, _, _, constrs) <- datas;
                        DataConstructor(cname, paramTypes) <- constrs;
                        (ty,ix) <- paramTypes.zipWithIndex;
                        cl <- transType(ty))
      yield (cname.name, "_" + ix) -> JavaLitType(cl)

    new LanguageMetaInfo(
      MultiDict.from(subtyps),
      Map.from(kidLinks),
      Map.from(litLinks)
    )
  }

  def transType(asScala: Type): Option[Class[_]] = asScala match {
    case TAny => Some(classOf[Any])
    case TNothing => Some(classOf[Nothing])
    case TTuple(ts) =>  None
    case TData(name) => None
    case TScalaBoolean => Some(classOf[Boolean])
    case TScalaInt => Some(classOf[Int])
    case TScalaLong => Some(classOf[Long])
    case TScalaDouble => Some(classOf[Double])
    case TScalaString => Some(classOf[String])
    case TScalaAny => Some(classOf[Any])
    case TScala(ty) => None
    case TOption(ty) => None
    case TSet(ty) => None
  }

}
