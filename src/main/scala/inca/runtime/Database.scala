package inca.runtime

import inca.runtime.Query.ChangeFeed
import inca.runtime.context.DataModel
import inca.runtime.context.DataModel.Link
import inca.runtime.index.MetaElements.PrimitiveValue
import inca.runtime.index._
import inca.runtime.index.binary.{BidirectionalManyToOneIndex, BidirectionalOneToOneIndex}
import inca.runtime.index.dynamic.{DynamicIndex, DynamicIndexFactory}
import inca.runtime.index.unary.{UnaryBagIndex, UnarySetIndex}
import inca.runtime.index.virtual.VirtualIndex
import org.eclipse.viatra.query.runtime.api.scope.{IBaseIndex, IIndexingErrorListener, IInstanceObserver, ViatraBaseIndexChangeListener}
import org.eclipse.viatra.query.runtime.matchers.context._
import org.eclipse.viatra.query.runtime.matchers.tuple.{ITuple, Tuple, TupleMask}
import org.eclipse.viatra.query.runtime.matchers.util.Accuracy
import truechange._

import java.util.Optional
import java.util.concurrent.Callable
import java.{lang, util}
import scala.collection.mutable
import scala.jdk.CollectionConverters._


class Database(
                _languageMetaInfo: DataModel,
                _dynamicIndices: Seq[DynamicIndexFactory],
                _metaContext: IQueryMetaContext
             )
  extends AbstractQueryRuntimeContext with IBaseIndex with ChangeFeed {

  def this() = this(null, Seq(), null)

  val languageMetaInfo: DataModel = if (_languageMetaInfo != null) _languageMetaInfo else new DataModel()

  override def getMetaContext: IQueryMetaContext = _metaContext




  /* indices */

  private[runtime] val nodeInstances: mutable.Map[Type, UnarySetIndex[URI]] = mutable.Map()
  private[runtime] val primitiveInstances: mutable.Map[LitType, UnaryBagIndex[PrimitiveValue]] = mutable.Map()
  private[runtime] val linkNodeInstances: mutable.Map[Link, BidirectionalOneToOneIndex[URI, URI]] = mutable.Map()
  private[runtime] val linkPrimitiveInstances: mutable.Map[Link, BidirectionalManyToOneIndex[URI, PrimitiveValue]] = mutable.Map()
  private[runtime] val linkListFirstInstances: BidirectionalOneToOneIndex[URI, URI] = new BidirectionalOneToOneIndex[URI,URI](LinkListFirstKey)
  private[runtime] val linkListNextInstances: BidirectionalOneToOneIndex[URI, URI] = new BidirectionalOneToOneIndex[URI,URI](LinkListNextKey)

  private[runtime] val namedRelationInstances: mutable.Map[String, BagIndex] = mutable.Map()



  private[runtime] val dynamicIndices: Map[DynamicKey, DynamicIndex] = _dynamicIndices.map { fact =>
    val ix = fact.makeIndex(this)
    ix.key.asInstanceOf[DynamicKey] -> ix
  }.toMap

  /** virtual indexes are added on demand */
  private[runtime] var virtualIndices: Map[VirtualKey, VirtualIndex] = Map()

  @inline
  private[runtime] def nodeInstancesEnsure(ty: Type) = nodeInstances.getOrElse(ty, {
    val ix = new UnarySetIndex[URI](NodeTypeKey(ty))
    nodeInstances += ty -> ix
    ix
  })

  @inline
  private[runtime] def namedRelationInstancesEnsure(name: String, arity: Int) = namedRelationInstances.getOrElse(name, {
    val ix = new BagIndex(NamedRelationKey(name, arity))
    namedRelationInstances += name -> ix
    ix
  })

  @inline
  private[runtime] def primitiveInstancesEnsure(primitiveType: LitType) = primitiveInstances.getOrElse(primitiveType, {
    val ix = new UnaryBagIndex[PrimitiveValue](PrimitiveTypeKey(primitiveType))
    primitiveInstances += primitiveType -> ix
    ix
  })

  @inline
  private[runtime] def linkNodeInstancesEnsure(link: Link) = linkNodeInstances.getOrElse(link, {
    val ix = new BidirectionalOneToOneIndex[URI, URI](LinkNodeKey(link))
    linkNodeInstances += link -> ix
    ix
  })

  @inline
  private[runtime] def linkPrimitiveInstancesEnsure(link: Link) = linkPrimitiveInstances.getOrElse(link, {
    val ix = new BidirectionalManyToOneIndex[URI, PrimitiveValue](LinkPrimitiveKey(link))
    linkPrimitiveInstances += link -> ix
    ix
  })

  private[runtime] def virtualIndexEnsure(key: VirtualKey) = virtualIndices.getOrElse(key, {
    val ix = key.factory.makeIndex(key, this)
    virtualIndices += key -> ix
    ix
  })


  /* BaseIndex listeners */

  private val baseIndexListeners: mutable.Set[ViatraBaseIndexChangeListener] = mutable.Set()
  override def addBaseIndexChangeListener(listener: ViatraBaseIndexChangeListener): Unit = baseIndexListeners += listener
  override def removeBaseIndexChangeListener(listener: ViatraBaseIndexChangeListener): Unit = baseIndexListeners -= listener
  def notifyBaseIndexListeners(): Unit = baseIndexListeners.foreach(_.notifyChanged(true))






  /** Process edit scripts */

  private val updater: DatabaseUpdater = new DatabaseUpdater(this)

  // process coreedits to so that first dynamic indicies are modified and then the core indicies
  // we want to avoid interleaving this
  override def processEditScript(edits: EditScript): Unit = {
    dynamicIndices.values.foreach(_.startProcessEditScript())
    updater.startProcessEditScript()

    edits.coreEdits.foreach { edit =>
      dynamicIndices.values.foreach(_.processEdit(edit))
      updater.processEdit(edit)
    }

    dynamicIndices.values.foreach(_.endProcessEditScript())
    updater.endProcessEditScript()
  }

  override def insert(relName: String, tuple: Tuple): Unit =
    namedRelationInstancesEnsure(relName, tuple.getSize).insert(tuple)

  override def delete(relName: String, tuple: Tuple): Unit =
    namedRelationInstancesEnsure(relName, tuple.getSize).delete(tuple)

  def iterateNext(from: truechange.URI)(f: truechange.URI => Unit): Unit = {
    val index = linkListNextInstances.index
    f(from)
    var nextNode = index.get(from)
    while (nextNode != null) {
        f(nextNode)
        nextNode = index.get(nextNode)
    }
  }



  /* index delegation */

  @inline
  private[runtime] def getIndex(key: IInputKey): Option[Index] = key match {
    case NodeTypeKey(ty) => nodeInstances.get(ty)
    case PrimitiveTypeKey(primitiveType) => primitiveInstances.get(primitiveType)
    case LinkNodeKey(link) => linkNodeInstances.get(link)
    case LinkPrimitiveKey(link) => linkPrimitiveInstances.get(link)
    case LinkListFirstKey => Some(linkListFirstInstances)
    case LinkListNextKey => Some(linkListNextInstances)
    case NamedRelationKey(name, _) => namedRelationInstances.get(name)
    case dkey: DynamicKey => Some(dynamicIndices(dkey))
    case vkey: VirtualKey => Some(virtualIndexEnsure(vkey))
    case _ => throw new IllegalArgumentException(s"Unknown input key $key")
  }


  @inline
  private[runtime] def ensureIndex(key: IInputKey): Index = key match {
    case NodeTypeKey(ty) => nodeInstancesEnsure(ty)
    case PrimitiveTypeKey(primitiveType) => primitiveInstancesEnsure(primitiveType)
    case LinkNodeKey(link) => linkNodeInstancesEnsure(link)
    case LinkPrimitiveKey(link) => linkPrimitiveInstancesEnsure(link)
    case LinkListFirstKey => linkListFirstInstances
    case LinkListNextKey => linkListNextInstances
    case NamedRelationKey(name, arity) => namedRelationInstancesEnsure(name, arity)
    case dkey: DynamicKey => dynamicIndices(dkey)
    case vkey: VirtualKey => virtualIndexEnsure(vkey)
    case _ => throw new IllegalArgumentException(s"Unknown input key $key")
  }

  override def countTuples(key: IInputKey, mask: TupleMask, seed: ITuple): Int = getIndex(key) match {
    case Some(ix) => ix.countTuples(mask, seed)
    case None => 0
  }

  override def enumerateTuples(key: IInputKey, mask: TupleMask, seed: ITuple): lang.Iterable[Tuple] = getIndex(key) match {
    case Some(ix) => ix.enumerateTuples(mask, seed).asJava
    case None => util.Collections.emptyList()
  }

  override def enumerateValues(key: IInputKey, mask: TupleMask, seed: ITuple): lang.Iterable[_] = getIndex(key) match {
    case Some(ix) => ix.enumerateValues(mask, seed).asJava
    case None => util.Collections.emptyList()
  }

  override def containsTuple(key: IInputKey, seed: ITuple): Boolean = getIndex(key) match {
    case Some(ix) => ix.containsTuple(seed)
    case None => false
  }

  override def addUpdateListener(key: IInputKey, seed: Tuple, listener: IQueryRuntimeContextListener): Unit =
    ensureIndex(key).addListener(listener, seed)

  override def removeUpdateListener(key: IInputKey, seed: Tuple, listener: IQueryRuntimeContextListener): Unit = getIndex(key) match {
    case Some(ix) => ix.removeListener(listener, seed)
    case None =>
  }

  override def isIndexed(key: IInputKey, service: IndexingService): Boolean =
    getIndex(key).nonEmpty

  override def ensureIndexed(key: IInputKey, service: IndexingService): Unit =
    getIndex(key).getOrElse(throw new RuntimeException(s"Not indexed key $key"))






  /* Unused stuff required by Viatra IQueryRuntimeContext */

  override def ensureWildcardIndexing(service: IndexingService): Unit = { }
  override def estimateCardinality(key: IInputKey, groupMask: TupleMask, requiredAccuracy: Accuracy): Optional[lang.Long] = Optional.empty()

  override def wrapElement(externalElement: Any): Any = externalElement
  override def unwrapElement(internalElement: Any): Any = internalElement
  override def wrapTuple(externalElements: Tuple): Tuple = externalElements
  override def unwrapTuple(internalElements: Tuple): Tuple = internalElements

  override def isCoalescing: Boolean = false
  override def coalesceTraversals[V](callable: Callable[V]): V = callable.call()
  override def executeAfterTraversal(runnable: Runnable): Unit = runnable.run()





  /* Unused stuff required by Viatra IBaseIndex */

  override def resampleDerivedFeatures(): Unit = { }
  override def addIndexingErrorListener(listener: IIndexingErrorListener): Boolean = false
  override def removeIndexingErrorListener(listener: IIndexingErrorListener): Boolean = false
  override def addInstanceObserver(observer: IInstanceObserver, observedObject: Any): Boolean = false
  override def removeInstanceObserver(observer: IInstanceObserver, observedObject: Any): Boolean = false
}
