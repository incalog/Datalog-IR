package inca.viatra.runtime.db.updater

import inca.viatra.runtime.db.Database
import truechange.*

class DirectDatabaseUpdater(val db: Database) extends DatabaseUpdater {
  def startProcessEditScript(): Unit = {
    /* do nothing */
  }

  def endProcessEditScript(): Unit = {
    /* do nothing */
  }

  def processEdit(edit: CoreEdit): Unit = edit match {
    case Update(node, NamedTag(tagname), oldlits, newlits) =>
      // delete lits from primitiveInstances and links from node to lits
      var newLitsMap = newlits.toMap
      oldlits.foreach { case (k, oldLit) =>
        newLitsMap.get(k) match {
          case Some(newLit) =>
            newLitsMap -= k
            if (oldLit != newLit) {
              db.primitiveInstances(JavaLitType(oldLit.getClass)).delete(oldLit)
              db.linkPrimitiveInstances(tagname -> k).delete(node, oldLit)
              db.primitiveInstancesEnsure(JavaLitType(newLit.getClass)).insert(newLit)
              db.linkPrimitiveInstances(tagname -> k).insert(node, newLit)
            }
          case None =>
            db.primitiveInstances(JavaLitType(oldLit.getClass)).delete(oldLit)
            db.linkPrimitiveInstances(tagname -> k).delete(node, oldLit)
        }
      }
      newLitsMap.foreach { case (k, newLit) =>
        db.primitiveInstancesEnsure(JavaLitType(newLit.getClass)).insert(newLit)
        db.linkPrimitiveInstances(tagname -> k).insert(node, newLit)
      }

    // delete link, leave rest intact
    case Detach(node, _, link, parent, ptag) => link.getRawLink match {
      case NamedLink(linkname) => ptag match {
        case NamedTag(tagname) => db.linkNodeInstances(tagname -> linkname).delete(parent, node)
        case ListTag(_) => editError(s"Cannot detach link $linkname from list $ptag. " + edit)
      }
      case ListFirstLink(_) => db.linkListFirstInstances.delete(parent, node)
      case ListNextLink(_) => db.linkListNextInstances.delete(parent, node)
    }

    // add link, leave rest intact
    case Attach(node, _, link, parent, ptag) => link.getRawLink match {
      case NamedLink(linkname) => ptag match {
        case NamedTag(tagname) => db.linkNodeInstancesEnsure(tagname -> linkname).insert(parent, node)
        case ListTag(_) => editError(s"Cannot attach link $linkname from list $ptag. " + edit)
      }
      case ListFirstLink(_) => db.linkListFirstInstances.insert(parent, node)
      case ListNextLink(_) => db.linkListNextInstances.insert(parent, node)
    }

    case Load(node, ListTag(ty), kids, lits) =>
      // insert node to nodeInstances (also for supertypes)
      val lty = ListType(ty)
      for (sup <- Iterable(lty) ++ db.dataModel.supertypes(lty)) {
        db.nodeInstancesEnsure(sup).insert(node)
      }
      if (kids.nonEmpty || lits.nonEmpty)
        editError("Lists cannot have kids or lits. " + edit)
    case Load(node, NamedTag(tagname), kids, lits) =>
      // insert node to nodeInstances (also for supertypes)
      val nty = SortType(tagname)
      for (sup <- Iterable(nty) ++ db.dataModel.supertypes(nty)) {
        db.nodeInstancesEnsure(sup).insert(node)
      }
      // insert links from node to kids
      for ((name, kid) <- kids) {
        db.linkNodeInstancesEnsure(tagname -> name).insert(node, kid)
      }
      // insert lits to primitiveInstances and links from node to lits
      for ((name, lit) <- lits) {
        val litTy = JavaLitType(lit.getClass)
        db.primitiveInstancesEnsure(litTy).insert(lit)
        db.linkPrimitiveInstancesEnsure(tagname -> name).insert(node, lit)
      }

    case Unload(node, ListTag(ty), kids, lits) =>
      // delete node from nodeInstances (also for supertypes)
      val lty = ListType(ty)
      for (sup <- Iterable(lty) ++ db.dataModel.supertypes(lty)) {
        db.nodeInstances(sup).delete(node)
      }
      if (kids.nonEmpty || lits.nonEmpty)
        editError("Lists cannot have kids or lits. " + edit)
    case Unload(node, NamedTag(tagname), kids, lits) =>
      // delete node from nodeInstances (also for supertypes)
      val nty = SortType(tagname)
      for (sup <- Iterable(nty) ++ db.dataModel.supertypes(nty)) {
        db.nodeInstances(sup).delete(node)
      }
      // delete links from node to kids
      for ((name, kid) <- kids) {
        db.linkNodeInstances(tagname -> name).delete(node, kid)
      }
      // delete lits from primitiveInstances and links from node to lits
      for ((name, lit) <- lits) {
        val litTy = JavaLitType(lit.getClass)
        db.primitiveInstances(litTy).delete(lit)
        db.linkPrimitiveInstances(tagname -> name).delete(node, lit)
      }
  }
}

