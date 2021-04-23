package inca.debugger

import inca.frontend.core.tree._
import inca.runtime.DatabaseAccessor
import truechange.{AnyType, ListType, LitType, NothingType, SortType}

private object Util {

  private[debugger] def getNodeInstances(db: DatabaseAccessor, ty: truechange.Type = AnyType): Set[ColumnValue] = {
    db.nodeInstances.get(ty) match {
      case Some(vs) => vs.entries.map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)).toSet
      case None => Set()
    }
  }

  private[debugger] def getPrimitiveInstances(db: DatabaseAccessor, litType: Option[LitType] = None): Set[ColumnValue] = litType match {
    case Some(ty) =>
      db.primitiveInstances.get(ty) match {
        case Some(vs) => vs.entries.map(v => ScalaValue(v)).toSet
        case None => Set()
      }
    case None =>
      db.primitiveInstances.values.flatMap(ind => ind.entries.map(v => ScalaValue(v))).toSet
  }

  private[debugger] def getLinks(db: DatabaseAccessor, uriValue: URIValue, link: Link): Set[ColumnValue] = link match {
    case link: CoreLink => link match {
      case NamedLink(field) =>
        val lnkNodes = db.linkNodeInstances.filter {
          case (lnk, uris) => lnk._2 == field.name && uris.index.containsKey(uriValue.uri)
        }
        if(lnkNodes.isEmpty) {
          db.linkPrimitiveInstancesByValue1(uriValue.uri).filter {
            case (lnk, _) => lnk._2 == field.name
          }.flatMap {
            case (_, vals) => vals.index(uriValue.uri).toSeq.map(prim => ScalaValue(prim))
          }.toSet

        } else {
          lnkNodes.map {
            case (_, uris) =>
              val uri = uris.index.get(uriValue.uri)
              URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)
          }.toSet
        }

      case ParentLink =>
        val linkNodes = db.linkNodeInstancesByValue2(uriValue.uri)
        linkNodes.flatMap {
          case (_, uris) =>
            val parentUris = uris.indexInverted(uriValue.uri)
            parentUris.map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq))
        }.toSet

      case ChildrenLink =>
        val linkNodes = db.linkNodeInstancesByValue1(uriValue.uri)
        if(linkNodes.nonEmpty) {
          linkNodes.flatMap {
            case (_, uris) =>
              val childUris = uris.index(uriValue.uri)
              childUris.map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq))
          }.toSet

        } else {
          val llFirst = db.linkListFirstInstances.index(uriValue.uri)
            .map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)).toSeq

          if(llFirst.isEmpty) Set()
          else (llFirst ++ getLinkListRest(db, llFirst.head)).toSet
        }

      case NextLink =>
        val llNext = db.linkListNextInstances.index(uriValue.uri)
          .map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)).toSeq
        llNext.toSet

      case PreviousLink =>
        val llPrev = db.linkListNextInstances.indexInverted(uriValue.uri)
          .map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)).toSeq
        llPrev.toSet

      case SizeLink =>
        val llFirst = db.linkListFirstInstances.index(uriValue.uri)
          .map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq)).toSeq

        if(llFirst.isEmpty) Set(ScalaValue(0))
        else Set(ScalaValue(1 + getLinkListRest(db, llFirst.head).size))

      case _ => Set()
    }
    case _ => Set()
  }

  private def getLinkListRest(db: DatabaseAccessor, uriValue: URIValue): Seq[ColumnValue] = {
    val nxt = db.linkListNextInstances.index(uriValue.uri)
      .map(uri => URIValue(uri, db.nodeInstancesByValue(uri).keys.toSeq))

    if(nxt.isEmpty) Seq()
    else (nxt ++ getLinkListRest(db, nxt.head)).toSeq
  }

  private[debugger] def getLitVal(sv: Any): Any = sv match {
    case BooleanLiteral(v) => v
    case IntLiteral(v) => v
    case LongLiteral(v) => v
    case DoubleLiteral(v) => v
    case StringLiteral(v) => v
    case UnitLiteral => UnitLiteral
  }

  private[debugger] def toTType(ty: truechange.Type): Type = ty match {
    case NothingType => TNothing
    case AnyType => TAny
    case SortType(name) => TNode(name)
    case ListType(ty) => TList(TNode(ty.toString))
  }

  private[debugger] def toTScala(v: Any): TScala = v match {
    case _: Boolean => TScalaBoolean
    case _: Int => TScalaInt
    case _: Long => TScalaLong
    case _: Double => TScalaDouble
    case _: String => TScalaString
    case _: Any => TScalaAny
  }
}
