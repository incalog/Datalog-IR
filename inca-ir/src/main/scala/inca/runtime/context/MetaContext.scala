package inca.runtime.context

import inca.runtime.index.*
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.SizeIndex
import org.eclipse.viatra.query.runtime.matchers.context.common.JavaTransitiveInstancesKey
import org.eclipse.viatra.query.runtime.matchers.context.{AbstractQueryMetaContext, IInputKey, InputKeyImplication}

import java.util
import java.util.Collections
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

import truechange.*

class MetaContext(langMetaInfo: DataModel) extends AbstractQueryMetaContext:
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

    case _: PrimitiveTypeKey =>
      Collections.emptySet()

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
      val impliedParent = ParentIndex.Key
      Seq(
        new InputKeyImplication(key, impliedSource, Collections.singletonList(0)),
        new InputKeyImplication(key, impliedTarget, Collections.singletonList(1)),
        new InputKeyImplication(key, impliedParent, util.Arrays.asList(1, 0))
      ).asJava


    case LinkPrimitiveKey(link@(tagname, _)) =>
      val impliedSource = NodeTypeKey(SortType(tagname))
      val impliedTarget = PrimitiveTypeKey(langMetaInfo.litLinks(link))
      Seq(
        new InputKeyImplication(key, impliedSource, Collections.singletonList(0)),
        new InputKeyImplication(key, impliedTarget, Collections.singletonList(1))
      ).asJava

    case _: VirtualKey =>
      Collections.emptySet()
    case _: NamedRelationKey =>
      Collections.emptySet()
    case ParentIndex.Key =>
      Collections.emptySet()
    case LinkListFirstKey =>
      val impliedParent = ParentIndex.Key
      Seq(
        new InputKeyImplication(key, impliedParent, util.Arrays.asList(1, 0))
      ).asJava
    case LinkListNextKey =>
      Collections.emptySet()
  }

  val bidirectional: util.Map[util.Set[Integer], util.Set[Integer]] = util.Map.of(
    Collections.singleton(0), Collections.singleton(1),
    Collections.singleton(1), Collections.singleton(0)
  )
  val leftToRight: util.Map[util.Set[Integer], util.Set[Integer]] = util.Map.of(
    Collections.singleton(0), Collections.singleton(1)
  )

  override def getFunctionalDependencies(key: IInputKey): util.Map[util.Set[Integer], util.Set[Integer]] = key match {
    case _: LinkNodeKey | LinkListFirstKey | LinkListNextKey =>
      bidirectional
    case _: LinkPrimitiveKey | ParentIndex.Key | SizeIndex.Key =>
      leftToRight
    case _ => Collections.emptyMap()
  }
