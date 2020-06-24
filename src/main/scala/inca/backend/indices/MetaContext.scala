package inca.backend.indices

import java.util
import java.util.Collections

import inca.MetaElements
import inca.MetaElements.{DefinedNodeLink, ListFirstLink, ListNextLink, ListType, NamedLink, NodeType, PrimitiveType, VirtualLink}
import InputKey.{LinkKey, NodeTypeKey, PrimitiveKey}
import inca.backend.virtual.VirtualKey
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import org.eclipse.viatra.query.runtime.matchers.context.{AbstractQueryMetaContext, IInputKey, InputKeyImplication}

import scala.jdk.CollectionConverters._

class MetaContext(langMetaInfo: LanguageMetaInfo) extends AbstractQueryMetaContext {
  override def isEnumerable(key: IInputKey): Boolean = true
  override def isStateless(key: IInputKey): Boolean = false

  override def getImplications(implyingKey: IInputKey): util.Collection[InputKeyImplication] = implyingKey match {
    case key: NodeTypeKey =>
      key.`type` match {
        case NodeType(name) =>
          val supers = langMetaInfo.directSupertypes(name)
          supers.map { stype =>
            val impliedSuper = new NodeTypeKey(NodeType(stype))
            new InputKeyImplication(key, impliedSuper, Collections.singletonList(0))
          }.asJava
        case ListType(name) =>
          // TODO should lists be invariant or covariant?
          Collections.emptySet()
      }
    case _: JavaTransitiveInstancesKey =>
      throw new IllegalStateException("TODO currently do not support eval nodes hence no javatranskey")
    case key: LinkKey =>
      key.`type` match {
        case ListFirstLink(typ) =>
          val firstImpl = new InputKeyImplication(key, new NodeTypeKey(typ), Collections.singletonList(0))
          val secondImpl = typ.contained match {
            case MetaElements.ListType(contained) =>
              new InputKeyImplication(key, new NodeTypeKey(ListType(contained)), Collections.singletonList(1))
            case MetaElements.NodeType(name) =>
              new InputKeyImplication(key, new NodeTypeKey(NodeType(name)), Collections.singletonList(1))
            case PrimitiveType(name) => throw new IllegalArgumentException("TODO support implication for primitive type in lists")
          }
          Seq(firstImpl, secondImpl).asJava
        case NamedLink(typ, field) =>
          val impliedSource = new NodeTypeKey(typ)
          val trgType = langMetaInfo.links(typ.name)(field)
          val firstImpl = new InputKeyImplication(key, impliedSource, Collections.singletonList(0))
          // TODO distinct between primitive, list and nodetype
          val secondImpl =
            if (false) throw new IllegalArgumentException("TODO support implications for other than nodetype")
            else new InputKeyImplication(key, new NodeTypeKey(NodeType(trgType)), Collections.singletonList(1))
          Seq(firstImpl, secondImpl).asJava
        case ListNextLink() => Seq().asJava
          // TODO
        case DefinedNodeLink(typ, field) => Seq().asJava
          // do not know yet
        case link: VirtualLink => Seq().asJava
      }
    case key: PrimitiveKey =>
      Collections.emptySet()
    case key: VirtualKey =>
      Collections.emptySet()
    case _ => throw new IllegalArgumentException("Cannot support implication for: " + implyingKey)
  }

  override def getFunctionalDependencies(key: IInputKey): util.Map[util.Set[Integer], util.Set[Integer]] = key match {
    case _: LinkKey =>
      Collections.singletonMap(Collections.singleton(0), Collections.singleton(1))
    case _ => Collections.emptyMap()
  }
}
