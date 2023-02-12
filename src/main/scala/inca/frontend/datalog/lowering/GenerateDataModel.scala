package inca.frontend.datalog.lowering

import inca.frontend.datalog.syntax._
import inca.runtime.context.DataModel
import truechange.{JavaLitType, SortType}

import scala.collection.immutable.MultiDict

class GenerateDataModel(module: Module) {

  def transModule(): DataModel = {
    val datas = module.content.collect { case data: DataDef => data }

    val subtyps = for (DataDef(_, name, constrs) <- datas;
                       DataConstructor(cname, _) <- constrs)
    yield SortType(cname.name) -> SortType(name.name)
    val kidLinks = for (DataDef(_, _, constrs) <- datas;
                        DataConstructor(cname, params) <- constrs;
                        p <- params if p.typ.isInstanceOf[TData])
    yield (cname.name, p.name.name) -> SortType(p.typ.asInstanceOf[TData].name.name)
    val litLinks = for (DataDef(_, _, constrs) <- datas;
                        DataConstructor(cname, params) <- constrs;
                        p <- params;
                        cl <- transType(p.typ))
    yield (cname.name, p.name.name) -> JavaLitType(cl)
    val types = datas.map { d => SortType(d.name.name) }.toSet ++ subtyps.map(_._1)

    new DataModel(
      types,
      MultiDict.from(subtyps),
      Map.from(kidLinks),
      Map.from(litLinks)
    )
  }

  def transType(asScala: Type): Option[Class[_]] = asScala match {
    case TAny => Some(classOf[Any])
    case TNothing => Some(classOf[Nothing])
    case TData(_) => None
    case TLiteral(JavaLitType(cl)) => Some(cl)
    case TScalaBoolean => Some(classOf[Boolean])
    case TScalaInt => Some(classOf[Int])
    case TScalaLong => Some(classOf[Long])
    case TScalaDouble => Some(classOf[Double])
    case TScalaString => Some(classOf[String])
    case TScalaAny => Some(classOf[Any])
    case TScala(_) => None
  }

}
