package inca.frontend.objectoriented.lowering

import inca.backend.ir.Datalog
import inca.frontend.objectoriented.core._
import inca.runtime.context.DataModel
import truechange.{JavaLitType, LitType, SortType}

import scala.collection.immutable.MultiDict

class GenerateDataModel(module: Module) {

  def transModule(): DataModel = {
    val types = module.classes.map(c => SortType(c.name.raw)).toSet + SortType("Null")

    val classHierachies = module.classes.flatMap { c =>
      c.parentClassRefs.map { p =>
        SortType(c.name.raw) -> SortType(p.name.raw)
      } :+ SortType("Null") -> SortType(c.name.raw)
    }

    // (cls, idx) => field.typ if field.typ.isInstanceOf[TClass]
    val kidLinks = module.classes.flatMap { c =>
      c.content.zipWithIndex.flatMap {
        case (FieldDef(_, _, _, TClass(ref), _, _), idx) =>
          Some((c.name.raw, "_" + idx) -> SortType(ref.name.raw))
        case _ => None
      }
    }

    // (cls, idx) => transType(field.typ) if transType(field.typ) != None
    val litLinks = module.classes.flatMap { c =>
      c.content.zipWithIndex.flatMap {
        case (FieldDef(_, _, _, ty, _, _), idx) if transType(ty).isDefined =>
          Some((c.name.raw, "_" + idx) -> transType(ty).get)
        case _ => None
      }
    }

    new DataModel(
      types,
      MultiDict.from(classHierachies),
      Map.from(kidLinks),
      Map.from(litLinks)
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
    case TSet(_) => None
    case TName(_) => None
  }

}
