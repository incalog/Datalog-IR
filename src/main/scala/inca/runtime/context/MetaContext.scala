package inca.runtime.context

import java.util
import java.util.Collections

import inca.runtime.index._
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import org.eclipse.viatra.query.runtime.matchers.context.{AbstractQueryMetaContext, IInputKey, InputKeyImplication}
import truechange.SortType

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

class MetaContext(langMetaInfo: DataModel) extends AbstractQueryMetaContext {
  override def isEnumerable(key: IInputKey): Boolean = key.isEnumerable
  override def isStateless(key: IInputKey): Boolean = key match {
    case key: IndexKey[_] => key.isStateless
    case _ => false
  }

  override def getImplications(key: IInputKey): util.Collection[InputKeyImplication] = key match {
    case NodeTypeKey(ty) =>
      val supers = langMetaInfo.directSupertypes(ty)
      supers.map { stype =>
        val impliedSuper = NodeTypeKey(stype)
        new InputKeyImplication(key, impliedSuper, Collections.singletonList(0))
      }.toSeq.asJava

    case jkey: JavaTransitiveInstancesKey =>
      val instanceClass = jkey.getInstanceClass
      if (instanceClass != null) { // resolution successful
        // direct Java superClass
        val superClass = instanceClass.getSuperclass
        val result: ListBuffer[InputKeyImplication] = ListBuffer()
        if (superClass != null) {
          val impliedSuper = new JavaTransitiveInstancesKey(superClass)
          result += new InputKeyImplication(key, impliedSuper, util.Arrays.asList(0))
        }
        // direct Java superInterfaces
        for (superInterface <- instanceClass.getInterfaces) {
          if (superInterface != null) {
            val impliedInterface = new JavaTransitiveInstancesKey(superInterface)
            result += new InputKeyImplication(key, impliedInterface, util.Arrays.asList(0))
          }
        }
        result.asJavaCollection
      } else {
        Collections.emptySet()
      }


    case LinkNodeKey(link@(tagname, _)) =>
      val impliedSource = NodeTypeKey(SortType(tagname))
      val impliedTarget = NodeTypeKey(langMetaInfo.links(link))
      Seq(
        new InputKeyImplication(key, impliedSource, Collections.singletonList(0)),
        new InputKeyImplication(key, impliedTarget, Collections.singletonList(1))
      ).asJava
    case LinkPrimitiveKey(link@(tagname, _)) =>
      val impliedSource = NodeTypeKey(SortType(tagname))
      val impliedTarget = PrimitiveTypeKey(langMetaInfo.litLinks(link))
      Seq(
        new InputKeyImplication(key, impliedSource, Collections.singletonList(0)),
        new InputKeyImplication(key, impliedTarget, Collections.singletonList(1))
      ).asJava

//    case LinkNodeKey(FirstLink(typ)) =>
//      val firstImpl = new InputKeyImplication(key, NodeTypeKey(typ), Collections.singletonList(0))
//      val secondImpl = typ match {
//        case MetaElements.ListType(contained) =>
//          new InputKeyImplication(key, new NodeTypeKey(ListType(contained)), Collections.singletonList(1))
//        case MetaElements.NodeType(name) =>
//          new InputKeyImplication(key, new NodeTypeKey(NodeType(name)), Collections.singletonList(1))
//      }
//      Seq(firstImpl, secondImpl).asJava
//    case LinkNodeKey(NextLink) =>
//      Seq().asJava
//      // TODO
//    case LinkNodeKey(DefinedNodeLink(typ, field)) =>
//      Seq().asJava
//      // TODO do not know yet

    case key: PrimitiveTypeKey =>
      Collections.emptySet()

    case key: DynamicKey =>
      Collections.emptySet()

    case key: VirtualKey =>
      Collections.emptySet()

    case _ => throw new IllegalArgumentException("Cannot support implication for: " + key)
  }

  override def getFunctionalDependencies(key: IInputKey): util.Map[util.Set[Integer], util.Set[Integer]] = key match {
    case _: LinkNodeKey =>
      Collections.singletonMap(Collections.singleton(0), Collections.singleton(1))
    case _ => Collections.emptyMap()
  }
}
