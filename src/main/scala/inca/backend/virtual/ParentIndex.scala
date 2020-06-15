package inca.backend.virtual

import inca.MetaElements.ParentLink
import org.eclipse.viatra.query.runtime.matchers.context.{IInputKey, IQueryRuntimeContextListener}
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask, Tuples}
import truechange.RootLink

import scala.collection.mutable

class ParentIndex extends VirtualIndex {
  private val listeners: mutable.Set[ParentListener] = mutable.Set()

  override var isDirty: Boolean = false

  override def isSupported(key: IInputKey): Boolean = key match {
    case VirtualKey(link: ParentLink) => true
    case _ => false
  }

  var parents: mutable.Map[truechange.Node, truechange.Node] = mutable.Map()

  override def processChange(change: truechange.Change): Unit = change match {
    case truechange.AttachNode(parent, link, node) =>
      link match {
        case RootLink => // do nothing because there is no designated root node
        case _ => insertParent(node, parent)
      }
    case truechange.DetachNode(parent, link, node, tag) =>
      link match {
        case RootLink => // do nothing because there is no designated root node
        case _ => deleteParent(node, parent)
      }
    case truechange.LoadNode(node, tag, kids, lits) =>
      kids.foreach { case (_, kid) =>
        insertParent(kid, node)
      }
      // we do not add a parent for literals because they are not unique (have no nodeuri)
    case truechange.UnloadNode(node, tat, kids, lits) =>
      kids.foreach { case (_, kid) =>
        deleteParent(kid, node)
      }
  }


  private def insertParent(node: truechange.Node, parent: truechange.Node): Unit = {
    if(parents.isDefinedAt(node)) {
      throw new RuntimeException("Already defined parent of " + node)
    } else {
      parents(node) = parent
      notifyParentListener(node, parent, isInsert = true)
    }
  }

  private def deleteParent(node: truechange.Node, parent: truechange.Node): Unit = {
    if(!parents.isDefinedAt(node)) {
      throw new RuntimeException("Unknown parent of " + node)
    } else {
      parents.remove(node)
      notifyParentListener(node, parent, isInsert = false)
    }
  }

  def notifyParentListener(node: truechange.Node, parent: truechange.Node, isInsert: Boolean): Unit = {
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
    if (mask.indices.length == 0) {
      parents
    } else throw new IllegalArgumentException("Must have exactly one unseeded element in enumerateValues() invocation, received instead: " + seed);
  }

  override def containsTuple(tuple: ITuple): Boolean = {
    val src = getFromTuple(tuple, 0).asInstanceOf[truechange.Node]
    val trg = getFromTuple(tuple, 0).asInstanceOf[truechange.Node]
    if (parents.isDefinedAt(src)) {
      parents(src) == trg
    } else {
      false
    }
  }

  private def getFromTuple(tuple: ITuple, index: Int): Object =
    if (tuple == null) null else tuple.get(index)


  override def addListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listeners += new ParentListener(listener, seed.get(0).asInstanceOf[truechange.Node], seed.get(1).asInstanceOf[truechange.Node])
  }
  override def removeListener(listener: IQueryRuntimeContextListener, seed: Tuple): Unit = {
    listeners -= new ParentListener(listener, seed.get(0).asInstanceOf[truechange.Node], seed.get(1).asInstanceOf[truechange.Node])
  }
}
