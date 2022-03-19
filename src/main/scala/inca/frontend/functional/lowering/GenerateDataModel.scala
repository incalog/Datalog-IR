package inca.frontend.functional.lowering

import inca.backend.ir.Datalog
import inca.frontend.functional.core._
import inca.runtime.context.DataModel
import truechange.{JavaLitType, LitType, SortType}

import scala.collection.immutable.MultiDict

class GenerateDataModel(module: Module) {

  def transModule(): DataModel = {
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
                        litType <- transType(ty))
      yield (cname.name, "_" + ix) -> litType
    val types = datas.map { d => SortType(d.name.name) }.toSet ++ subtyps.map(_._1)

    new DataModel(
      types,
      MultiDict.from(subtyps),
      Map.from(kidLinks),
      Map.from(litLinks)
    )
  }

  def transType(asScala: Type): Option[LitType] = asScala match {
    case TAny => Some(JavaLitType(classOf[Any]))
    case TNothing => Some(JavaLitType(classOf[Nothing]))
    case TScalaBoolean => Some(Datalog.TLiteral.Bool.litType)
    case TScalaInt => Some(Datalog.TLiteral.Int.litType)
    case TScalaLong => Some(Datalog.TLiteral.Long.litType)
    case TScalaDouble => Some(Datalog.TLiteral.Double.litType)
    case TScalaString => Some(Datalog.TLiteral.String.litType)
    case TScalaAny => Some(JavaLitType(classOf[Any]))
    case TFun(_, _) => None
    case TTuple(_) =>  None
    case TData(_) => None
    case TScala(_) => None
    case TOption(_) => None
    case TSet(_) => None
  }

}
