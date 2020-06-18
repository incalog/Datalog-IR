package inca.backend.indices

import java.util
import java.util.Collections

import inca.MetaElements
import inca.MetaElements.{Primitive, Node}
import inca.backend.indices.TFInputKey.{DataTypeKey, NodeLinkKey, NodeTypeKey}
import inca.backend.virtual.VirtualKey
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import org.eclipse.viatra.query.runtime.matchers.context.{AbstractQueryMetaContext, IInputKey, InputKeyImplication}

import scala.jdk.CollectionConverters._

class MetaContext(langMetaInfo: LanguageMetaInfo) extends AbstractQueryMetaContext {
  override def isEnumerable(key: IInputKey): Boolean = true
  override def isStateless(key: IInputKey): Boolean = false

  override def getImplications(implyingKey: IInputKey): util.Collection[InputKeyImplication] = implyingKey match {
    case key: NodeTypeKey =>
      val supers = langMetaInfo.directSupertypes(key.`type`.name)
      supers.map { stype =>
        // TODO why not a nodetypekey?
        val impliedSuper = new JavaTransitiveInstancesKey(stype)
        new InputKeyImplication(key, impliedSuper, Collections.singletonList(0))
      }.asJava
    case key: JavaTransitiveInstancesKey =>
      val supers = langMetaInfo.directSupertypes(key.getInstanceClass.getCanonicalName)
      supers.map { stype =>
        // TODO why not a nodetypekey?
        val impliedSuper = new JavaTransitiveInstancesKey(stype)
        new InputKeyImplication(key, impliedSuper, Collections.singletonList(0))
      }.asJava
    case key: NodeLinkKey =>
      val nodeLink = key.`type`
      // TODO currently cast to nodetype
      val impliedSource = new NodeTypeKey(nodeLink.typ.asInstanceOf[Node])
      val linkType = langMetaInfo.links(nodeLink.typ.name)(nodeLink.field)
      val set = new util.HashSet[InputKeyImplication]()
      set.add(new InputKeyImplication(key, impliedSource, Collections.singletonList(0)))
      val implication =
        if (MetaElements.isPrimitiveDataType(linkType)) {
          new InputKeyImplication(key, new DataTypeKey(Primitive(linkType)), Collections.singletonList(1))
        } else {
          new InputKeyImplication(key, new NodeTypeKey(Node(linkType)), Collections.singletonList(1))
        }
      set.add(implication)
      set
    case key: DataTypeKey =>
      val implied = new JavaTransitiveInstancesKey(key.`type`.name)
      Collections.singleton(new InputKeyImplication(key, implied, Collections.singletonList(0)))
    case key: VirtualKey =>
      // TODO what to do here?
      Collections.emptySet()
    case _ => throw new IllegalArgumentException("Cannot support implication for: " + implyingKey)
  }

  override def getFunctionalDependencies(key: IInputKey): util.Map[util.Set[Integer], util.Set[Integer]] = key match {
    case _: NodeLinkKey =>
      // TODO check whether link is pointing to ListType
      Collections.singletonMap(Collections.singleton(0), Collections.singleton(1))
    case _ => Collections.emptyMap()
  }
}
