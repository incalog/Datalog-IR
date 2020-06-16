package inca.backend.virtual

import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, Tuples}
import truechange.{ListFirstLink, ListNextLink, NamedLink, RootLink}

import scala.collection.mutable

case object ParentKey extends VirtualKey {
  override val getUniqueID: String = "parent"
  override val getArity: Int = 2
  override val isEnumerable: Boolean = true
}

class ParentIndex extends VirtualIndex {
  private val listeners: mutable.Set[ParentListener] = mutable.Set()

  override var isDirty: Boolean = false

  var parents: mutable.Map[truechange.NodeURI, truechange.NodeURI] = mutable.Map()

  override def processChange(change: truechange.Change): Unit = change match {
    case truechange.Attach(parent, _, link, node, _) => link match {
      case _: RootLink.type  | _: ListNextLink | _: ListFirstLink => // nothing to do
      case _: NamedLink => insertParent(node, parent)
    }
    case truechange.Detach(parent, _, link, node, _) => link match {
      case _: RootLink.type  | _: ListNextLink | _: ListFirstLink => // nothing to do
      case _: NamedLink => insertParent(node, parent)
    }
    case truechange.Load(node, _, kids, _) =>
      kids.foreach { case (_, kid) =>
        insertParent(kid, node)
      }
      // we do not add a parent for literals because they are not unique (have no nodeuri)
    case truechange.Unload(node, _, kids, _) =>
      kids.foreach { case (_, kid) =>
        deleteParent(kid, node)
      }
  }


  private def insertParent(node: truechange.NodeURI, parent: truechange.NodeURI): Unit = {
    parents.put(node, parent) match {
      case Some(_) => // do nothing
      case None => notifyParentListener(node, parent, isInsert = true)
    }
  }

  private def deleteParent(node: truechange.NodeURI, parent: truechange.NodeURI): Unit = {
    parents.remove(node) match {
      case Some(_) => notifyParentListener(node, parent, isInsert = false)
      case None => // do nothing
    }
  }

  def notifyParentListener(node: truechange.NodeURI, parent: truechange.NodeURI, isInsert: Boolean): Unit = {
    isDirty |= listeners.nonEmpty
    listeners.foreach { l =>
      if (isInsert) l.insert(node, parent)
      else l.delete(node, parent)
    }
  }

  override def countTuples(mask: TupleMask, seed: ITuple): Int =
    if (mask.indices.length == 0) {
      parents.size
    } else if (containsTuple(seed)) {
      1
    } else {
      0
    }

  override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    if (mask.indices.length == 0) {
      parents.map { case (node, parent) => Tuples.staticArityFlatTupleOf(node, parent) }
    } else {
      val seedInst = mask.getValue(seed, 0)
      if (containsTuple(seed)) {
        Seq(Tuples.staticArityFlatTupleOf(seedInst))
      } else {
        Seq()
      }
    }
  }

  override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_] = {
    throw new IllegalArgumentException("TODO currently no support for enumerating values of parent index")
  }

  override def containsTuple(tuple: ITuple): Boolean = {
    val src = getFromTuple(tuple, 0).asInstanceOf[truechange.NodeURI]
    val trg = getFromTuple(tuple, 1).asInstanceOf[truechange.NodeURI]
    parents.get(src) match {
      case Some(storedTrg) => trg == storedTrg
      case None => false
    }
  }

  private def getFromTuple(tuple: ITuple, index: Int): Object =
    if (tuple == null) null else tuple.get(index)


  override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listeners += new ParentListener(listener, seed.get(0).asInstanceOf[truechange.NodeURI], seed.get(1).asInstanceOf[truechange.NodeURI])
  }
  override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listeners -= new ParentListener(listener, seed.get(0).asInstanceOf[truechange.NodeURI], seed.get(1).asInstanceOf[truechange.NodeURI])
  }
}
