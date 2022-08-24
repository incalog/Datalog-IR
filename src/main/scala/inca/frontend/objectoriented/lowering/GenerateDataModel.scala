package inca.frontend.objectoriented.lowering

import inca.backend.ir.Datalog
import inca.frontend.objectoriented.core._
import inca.runtime.context.DataModel
import truechange.{JavaLitType, LitType, SortType}

import scala.collection.immutable.MultiDict

class GenerateDataModel(module: Module) {

  // TODO: This can be optimized by caching
  def getClassHierachy(classMap: Map[Name, ClassDef], classRef: ClassRef): Seq[ClassRef] = {
    val parentClassRefs = classMap(classRef.name).parentClassRefs
    parentClassRefs ++ parentClassRefs.flatMap(getClassHierachy(classMap, _))
  }

  def transModule(): DataModel = {
    val types = module.classes.map(c => SortType(c.name.name)).toSet

    val classMap = module.classes.map(c => c.name -> c).toMap

    val classHierachies = module.classes.flatMap { c =>
      getClassHierachy(classMap, ClassRef(c.name)).map { ref =>
        SortType(c.name.name) -> SortType(ref.name.name)
      }
    }

    // kidLinks
    // Should this be: (cls, idx) => field.typ if field.typ.isInstanceOf[TClass]

    // litLinks
    // Should this be: (cls, idx) => transType(field.typ) if transType(field.typ) != None

    // TODO: kidLinks, litLinks

    new DataModel(
      types,
      MultiDict.from(classHierachies),
      Map(),
      Map()
    )
  }

  def transType(asScala: Type): Option[LitType] = asScala match {
    case TAny => Some(JavaLitType(classOf[Any]))
    case TNull => None
    case TTuple(_) =>  None
    case TClass(_) => None
    case TScalaBoolean => Some(Datalog.TLiteral.Bool.litType)
    case TScalaInt => Some(Datalog.TLiteral.Int.litType)
    case TScalaLong => Some(Datalog.TLiteral.Long.litType)
    case TScalaDouble => Some(Datalog.TLiteral.Double.litType)
    case TScalaString => Some(Datalog.TLiteral.String.litType)
    case TScalaAny => Some(JavaLitType(classOf[Any]))
    case TScala(_) => None
    //case TSet(ty) => None
  }

}
