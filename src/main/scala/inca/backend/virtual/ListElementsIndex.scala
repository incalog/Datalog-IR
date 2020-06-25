package inca.backend.virtual
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContextListener
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, Tuples}
import truechange.{Attach, Change, Detach, Load, Unload}

import scala.collection.mutable


case object ListElementsKey extends VirtualKey {
  override def getUniqueID: String = "elements"
  override def getArity: Int = 2
  override def isEnumerable: Boolean = true
}

class ListElementsIndex extends VirtualIndex {
  val listeners: mutable.Set[ListElementsListener] = mutable.Set()

  val elements: mutable.Map[truechange.NodeURI, mutable.Set[truechange.NodeURI]] = mutable.Map()
  val lists: mutable.Map[truechange.NodeURI, truechange.NodeURI] = mutable.Map()

  var isDirty: Boolean = false

  // TODO better design to avoid having multiple nexts maps (parentlist and this one)
  val nexts: mutable.Map[truechange.NodeURI, truechange.NodeURI] = mutable.Map()

  private def iterateNext(from: truechange.NodeURI)(f: truechange.NodeURI => Unit): Unit = {
    f(from)
    var nextNode = nexts.get(from)
    while(nextNode.isDefined) {
      val node = nextNode.get
      f(node)
      nextNode = nexts.get(node)
    }
  }

  override def processChange(change: Change): Unit = change match {
    case Attach(list, _, truechange.ListFirstLink(ty), node, _) =>
      lists(node) = list
      iterateNext(node) { next => insertElement(list, next) }
    case Attach(prev, ptag, truechange.ListNextLink(ty), node, _) =>
      val list = lists(prev)
      iterateNext(node) { next =>
        insertElement(list, next)
        lists(next) = list
      }
      nexts(prev) = node
    case Attach(_, _, _, _, _) => // do nothing
    case Detach(list, _, truechange.ListFirstLink(ty), node, _) =>
      lists -= node
      iterateNext(node) { next => deleteElement(list, next) }
    case Detach(prev, _, truechange.ListNextLink(ty), node, _) =>
      val list = lists.get(prev)
      lists.get(prev) match {
        case Some(list) =>
          iterateNext(node) { next =>
            deleteElement(list, next)
            lists -= next
          }
        case None => // do nothing because there is no list defined for prev
      }
    case Detach(_, _, _, _, _) => // do nothing
    case Unload(_, _, _, _) => // do nothing
    case Load(_, _, _, _) => // do nothing
  }

  protected def insertElement(list: truechange.NodeURI, element: truechange.NodeURI): Unit = {
    elements.get(list) match {
      case Some(elems) =>
        if (elems.add(element)) notifyElementsListener(list, element, isInsert = true)
      case None =>
        elements(list) = mutable.Set(element)
        notifyElementsListener(list, element, isInsert = true)
    }
  }

  protected def deleteElement(list: truechange.NodeURI, element: truechange.NodeURI): Unit = {
    elements.get(list) match {
      case Some(elems) =>
        if (elems.remove(element)) notifyElementsListener(list, element, isInsert = false)
      case None => // do nothing
    }
  }

  def notifyElementsListener(list: truechange.NodeURI, element: truechange.NodeURI, isInsert: Boolean): Unit = {
    isDirty |= !listeners.isEmpty
    listeners.foreach { l =>
      if (isInsert) l.insert(list, element)
      else l.delete(list, element)
    }
  }

  override def countTuples(mask: TupleMask, seed: ITuple): Int = {
    if (mask.indices.length == 0) {
      throw new IllegalArgumentException("ListElements cannot be unseeded")
    } else {
      elements(seed.get(0).asInstanceOf[truechange.NodeURI]).size
    }
  }

  override def enumerateTuples(mask: TupleMask, seed: ITuple): Iterable[Tuple] = {
    val isListBound = false
    val isElementBound = false
    if (mask.indices.length == 0) {
      elements.keySet.flatMap { list =>
        elements(list).map { elem => Tuples.staticArityFlatTupleOf(list, elem) }
      }
    } else {
      elements(seed.get(0).asInstanceOf[truechange.NodeURI]).map(Tuples.staticArityFlatTupleOf)
    }
  }

  override def enumerateValues(mask: TupleMask, seed: ITuple): Iterable[_] = {
    if (mask.indices.length == 0) {
      throw new IllegalArgumentException("ListElements cannot be unseeded")
    } else {
      elements(seed.get(0).asInstanceOf[truechange.NodeURI])
    }
  }

  override def containsTuple(tuple: ITuple): Boolean = {
    val src = getFromTuple(tuple, 0).asInstanceOf[truechange.NodeURI]
    val trg = getFromTuple(tuple, 1).asInstanceOf[truechange.NodeURI]
    elements.get(src) match {
      case Some(elems) => elems.contains(trg)
      case None => false
    }
  }

  private def getFromTuple(tuple: ITuple, index: Int): Object =
    if (tuple == null) null else tuple.get(index)

  override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listeners += new ListElementsListener(listener, seed.get(0).asInstanceOf[truechange.NodeURI], seed.get(1).asInstanceOf[truechange.NodeURI])
  }

  override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listeners -= new ListElementsListener(listener, seed.get(0).asInstanceOf[truechange.NodeURI], seed.get(1).asInstanceOf[truechange.NodeURI])
  }
}
