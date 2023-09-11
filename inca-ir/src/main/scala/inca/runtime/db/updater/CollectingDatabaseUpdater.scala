package inca.runtime.db.updater

import inca.runtime.db.Database
import inca.runtime.index.MetaElements.PrimitiveValue
import inca.runtime.index.binary.BinaryMapIndex
import inca.runtime.index.unary.UnaryIndex
import org.eclipse.collections.api.factory.{Maps, Sets}
import org.eclipse.collections.api.map.MutableMap
import org.eclipse.collections.api.set.MutableSet
import truechange.*

import scala.jdk.CollectionConverters.*

class CollectingDatabaseUpdater(val db: Database) extends DatabaseUpdater {
  private val deletionsLit: MutableSet[(UnaryIndex[PrimitiveValue], PrimitiveValue)] = Sets.mutable.empty()
  private val deletionsURI: MutableSet[(UnaryIndex[URI], URI)] = Sets.mutable.empty()
  private val deletionsURILit: MutableMap[(BinaryMapIndex[URI, PrimitiveValue], URI), PrimitiveValue] = Maps.mutable.empty()
  private val deletionsURIURI: MutableMap[(BinaryMapIndex[URI, URI], URI), URI] = Maps.mutable.empty()


  private def insertOrUpdate(ix: UnaryIndex[PrimitiveValue], target: PrimitiveValue): Unit = {
    val isDeleted = deletionsLit.remove(ix -> target)
    if (isDeleted) {
      // nothing, deletion and insertion cancel each other out
    } else {
      ix.insert(target)
    }
  }
  private def insertOrUpdate(ix: UnaryIndex[URI], target: URI): Unit = {
    val isDeleted = deletionsURI.remove(ix -> target)
    if (isDeleted) {
      // nothing, deletion and insertion cancel each other out
    } else {
      ix.insert(target)
    }
  }
  private def insertOrUpdate(ix: BinaryMapIndex[URI, PrimitiveValue], node: URI, target: PrimitiveValue): Unit = {
    val oldTarget = deletionsURILit.remove(ix -> node)
    if (oldTarget == null)
      ix.insert(node, target)
    else
      ix.update(node, oldTarget, target)
  }
  private def insertOrUpdate(ix: BinaryMapIndex[URI, URI], node: URI, target: URI): Unit = {
    val oldTarget = deletionsURIURI.remove(ix -> node)
    if (oldTarget == null)
      ix.insert(node, target)
    else
      ix.update(node, oldTarget, target)
  }

  def startProcessEditScript(): Unit = {
    if (deletionsLit.notEmpty())
      throw new IllegalStateException(s"Nonempty deletions at start of edit script processing")
    if (deletionsURI.notEmpty())
      throw new IllegalStateException(s"Nonempty deletions at start of edit script processing")
    if (deletionsURILit.notEmpty())
      throw new IllegalStateException(s"Nonempty deletions at start of edit script processing")
    if (deletionsURIURI.notEmpty())
      throw new IllegalStateException(s"Nonempty deletions at start of edit script processing")
  }

  def endProcessEditScript(): Unit = {
    deletionsLit.asScala.foreach { case (ix, v) => ix.delete(v) }
    deletionsLit.clear()
    deletionsURI.asScala.foreach { case (ix, v) => ix.delete(v) }
    deletionsURI.clear()
    deletionsURILit.entrySet().asScala.foreach { e => e.getKey._1.delete(e.getKey._2, e.getValue) }
    deletionsURILit.clear()
    deletionsURIURI.entrySet().asScala.foreach { e => e.getKey._1.delete(e.getKey._2, e.getValue) }
    deletionsURIURI.clear()
  }


  /** processes edit to update this index accordingly */
  def processEdit(edit: CoreEdit): Unit = edit match {
    case Update(node, NamedTag(tagname), oldlits, newlits) =>
      // delete lits from primitiveInstances and links from node to lits
      var newLitsMap = newlits.toMap
      oldlits.foreach { case (k, oldLit) =>
        newLitsMap.get(k) match {
          case Some(newLit) =>
            newLitsMap -= k
            if (oldLit != newLit) {
              db.primitiveInstances(JavaLitType(newLit.getClass)).insert(newLit)
              db.linkPrimitiveInstances(tagname->k).update(node, oldLit, newLit)
              deletionsLit.add(db.primitiveInstances(JavaLitType(oldLit.getClass)) -> oldLit)
            }
          case None =>
            deletionsLit.add(db.primitiveInstances(JavaLitType(oldLit.getClass)) -> oldLit)
            deletionsURILit.put(db.linkPrimitiveInstances(tagname->k) -> node, oldLit)
        }
      }
      newLitsMap.foreach { case (k, newLit) =>
        insertOrUpdate(db.primitiveInstances(JavaLitType(newLit.getClass)), newLit)
        insertOrUpdate(db.linkPrimitiveInstances(tagname->k), node, newLit)
      }

    // delete link, leave rest intact
    case Detach(node, _, link, parent, ptag) => link.getRawLink match {
      case NamedLink(linkname) => ptag match {
        case NamedTag(tagname) => deletionsURIURI.put(db.linkNodeInstances(tagname->linkname) -> parent, node)
        case ListTag(_) => editError(s"Cannot detach link $linkname from list $ptag. " + edit)
      }
      case ListFirstLink(_) => deletionsURIURI.put(db.linkListFirstInstances -> parent, node)
      case ListNextLink(_) => deletionsURIURI.put(db.linkListNextInstances -> parent, node)
    }

    // add link, leave rest intact
    case Attach(node, _, link, parent, ptag) => link.getRawLink match {
      case NamedLink(linkname) => ptag match {
        case NamedTag(tagname) => insertOrUpdate(db.linkNodeInstancesEnsure(tagname->linkname), parent, node)
        case ListTag(_) => editError(s"Cannot attach link $linkname from list $ptag. " + edit)
      }
      case ListFirstLink(_) => insertOrUpdate(db.linkListFirstInstances, parent, node)
      case ListNextLink(_) => insertOrUpdate(db.linkListNextInstances, parent, node)
    }

    case Load(node, ListTag(ty), kids, lits) =>
      // insert node to nodeInstances (also for supertypes)
      val lty = ListType(ty)
      for (sup <- Iterable(lty) ++ db.dataModel.supertypes(lty)) {
        insertOrUpdate(db.nodeInstancesEnsure(sup), node)
      }
      if (kids.nonEmpty || lits.nonEmpty)
        editError("Lists cannot have kids or lits. " + edit)
    case Load(node, NamedTag(tagname), kids, lits) =>
      // insert node to nodeInstances (also for supertypes)
      val nty = SortType(tagname)
      for (sup <- Iterable(nty) ++ db.dataModel.supertypes(nty)) {
        insertOrUpdate(db.nodeInstancesEnsure(sup), node)
      }
      // insert links from node to kids
      for ((name, kid) <- kids) {
        insertOrUpdate(db.linkNodeInstancesEnsure(tagname->name), node, kid)
      }
      // insert lits to primitiveInstances and links from node to lits
      for ((name, lit) <- lits) {
        val litTy = JavaLitType(lit.getClass)
        insertOrUpdate(db.primitiveInstancesEnsure(litTy), lit)
        insertOrUpdate(db.linkPrimitiveInstancesEnsure(tagname->name), node, lit)
      }

    case Unload(node, ListTag(ty), kids, lits) =>
      // delete node from nodeInstances (also for supertypes)
      val lty = ListType(ty)
      for (sup <- Iterable(lty) ++ db.dataModel.supertypes(lty)) {
        deletionsURI.add(db.nodeInstances(sup) -> node)
      }
      if (kids.nonEmpty || lits.nonEmpty)
        editError("Lists cannot have kids or lits. " + edit)
    case Unload(node, NamedTag(tagname), kids, lits) =>
      // delete node from nodeInstances (also for supertypes)
      val nty = SortType(tagname)
      for (sup <- Iterable(nty) ++ db.dataModel.supertypes(nty)) {
        deletionsURI.add(db.nodeInstances(sup) -> node)
      }
      // delete links from node to kids
      for ((name, kid) <- kids) {
        deletionsURIURI.put(db.linkNodeInstances(tagname->name) -> node, kid)
      }
      // delete lits from primitiveInstances and links from node to lits
      for ((name, lit) <- lits) {
        val litTy = JavaLitType(lit.getClass)
        deletionsLit.add(db.primitiveInstances(litTy) -> lit)
        deletionsURILit.put(db.linkPrimitiveInstances(tagname->name) -> node, lit)
      }
  }
}
