package inca.runtime.context

import java.util
import java.util.Collections

import inca.runtime.index.MetaElements._
import inca.runtime.index._
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import org.eclipse.viatra.query.runtime.matchers.context.{AbstractQueryMetaContext, IInputKey, InputKeyImplication}

import scala.jdk.CollectionConverters._

class MetaContext(langMetaInfo: LanguageMetaInfo) extends AbstractQueryMetaContext {
  override def isEnumerable(key: IInputKey): Boolean = true
  override def isStateless(key: IInputKey): Boolean = false

  override def getImplications(key: IInputKey): util.Collection[InputKeyImplication] = key match {
    case NodeTypeKey(ty) =>
      val supers = langMetaInfo.directSupertypes(ty)
      supers.map { stype =>
        val impliedSuper = NodeTypeKey(stype)
        new InputKeyImplication(key, impliedSuper, Collections.singletonList(0))
      }.toSeq.asJava

    case _: JavaTransitiveInstancesKey =>
      throw new IllegalStateException("TODO currently do not support eval nodes hence no javatranskey")

    case LinkKey(link@NamedLink(typ, _)) =>
      val impliedSource = NodeTypeKey(typ)
      val impliedTarget = langMetaInfo.links(link).mkKey
      Seq(
        new InputKeyImplication(key, impliedSource, Collections.singletonList(0)),
        new InputKeyImplication(key, impliedTarget, Collections.singletonList(1))
      ).asJava
    case LinkKey(FirstLink(typ)) =>
      val firstImpl = new InputKeyImplication(key, NodeTypeKey(typ), Collections.singletonList(0))
      val secondImpl = typ match {
        case MetaElements.ListType(contained) =>
          new InputKeyImplication(key, new NodeTypeKey(ListType(contained)), Collections.singletonList(1))
        case MetaElements.NodeType(name) =>
          new InputKeyImplication(key, new NodeTypeKey(NodeType(name)), Collections.singletonList(1))
      }
      Seq(firstImpl, secondImpl).asJava
    case LinkKey(NextLink) =>
      Seq().asJava
      // TODO
    case LinkKey(DefinedNodeLink(typ, field)) =>
      Seq().asJava
      // TODO do not know yet

    case key: PrimitiveTypeKey =>
      Collections.emptySet()

    case key: DynamicKey =>
      Collections.emptySet()

    case _ => throw new IllegalArgumentException("Cannot support implication for: " + key)
  }

  override def getFunctionalDependencies(key: IInputKey): util.Map[util.Set[Integer], util.Set[Integer]] = key match {
    case _: LinkKey =>
      Collections.singletonMap(Collections.singleton(0), Collections.singleton(1))
    case _ => Collections.emptyMap()
  }
}
