package inca.backend

import java.util
import java.util.Collections

import inca.MetaElements.{DataType, NodeType, ParentLink}
import inca.backend.indices.TFInputKey.{DataTypeKey, NodeLinkKey, NodeTypeKey}
import inca.backend.indices.{Indices, TFRuntimeContext}
import inca.backend.virtual.{ParentIndex, ParentKey, VirtualIndex}
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, TupleMask, Tuples}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truediff.Diffable

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class RuntimeContextTests extends AnyFunSuite {

  val num1 = Num(1)
  val num2 = Num(2)
  val num3 = Num(3)
  val mul = Mul(num1, num2)
  val add = Add(mul, num3)

  test("Type hierarchy check") {
    val indices = new Indices()
    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    // TODO implement new collection of type hierarchy
    // superTypes
//    assert(Indices.superTypeMap.get(classOf[Num]).contains(classOf[Exp]))
//    assert(Indices.superTypeMap.get(classOf[Add]).contains(classOf[Exp]))
//    assert(Indices.superTypeMap.get(classOf[Mul]).contains(classOf[Exp]))
//    assert(isEmptyOrNull(Indices.superTypeMap.get(classOf[Exp])))
//
//    // subTypes
//    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Num])))
//    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Add])))
//    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Mul])))
//    assert(Indices.subTypeMap.get(classOf[Exp]).containsAll(util.Arrays.asList(classOf[Num], classOf[Add], classOf[Mul])))

    indices.dispose()
  }

  test("NodeType instances") {
    val indices = new Indices()
    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    val context = new TFRuntimeContext(indices)

    context.enumerateTuples(new NodeTypeKey(NodeType(classOf[Exp])), emptyMask, null).
      asScala should be(empty)

    context.enumerateTuples(new NodeTypeKey(NodeType(classOf[Num])), emptyMask, null).
      asScala should contain allOf(t1(num1.uri), t1(num2.uri), t1(num3.uri))

    context.enumerateTuples(new NodeTypeKey(NodeType(classOf[Add])), emptyMask, null).
      asScala should contain(t1(add.uri))

    context.enumerateTuples(new NodeTypeKey(NodeType(classOf[Mul])), emptyMask, null).
      asScala should contain(t1(mul.uri))

    indices.dispose()
  }

  test("DataType instances") {
    val indices = new Indices()
    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    val context = new TFRuntimeContext(indices)

    context.enumerateTuples(new DataTypeKey(DataType(classOf[Integer])), emptyMask, null).
      asScala should contain allOf(t1(1), t1(2), t1(3))

    context.enumerateTuples(new DataTypeKey(DataType(classOf[String])), emptyMask, null).
      asScala should be(empty)

    context.enumerateTuples(new DataTypeKey(DataType(classOf[Boolean])), emptyMask, null).
      asScala should be(empty)

    indices.dispose()
  }

  test("NodeLink instances") {
    val indices = new Indices()
    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    val context = new TFRuntimeContext(indices)

    context.enumerateTuples(new NodeLinkKey(NodeType(classOf[Num])("n")), emptyMask, null).
      asScala should contain allOf(t2(num1.uri, 1), t2(num2.uri, 2), t2(num3.uri, 3))

    context.enumerateTuples(new NodeLinkKey(NodeType(classOf[Mul])("l")), emptyMask, null).
      asScala should contain only (t2(mul.uri, num1.uri))

    context.enumerateTuples(new NodeLinkKey(NodeType(classOf[Mul])("r")), emptyMask, null).
      asScala should contain only (t2(mul.uri, num2.uri))

    context.enumerateTuples(new NodeLinkKey(NodeType(classOf[Add])("l")), emptyMask, null).
      asScala should contain only (t2(add.uri, mul.uri))

    context.enumerateTuples(new NodeLinkKey(NodeType(classOf[Add])("r")), emptyMask, null).
      asScala should contain only (t2(add.uri, num3.uri))

    indices.dispose()
  }

  test("VirtualLink parent") {
    val virtualIndices = new java.util.HashSet[VirtualIndex]()
    virtualIndices.add(new ParentIndex())
    val indices = new Indices(null, virtualIndices)
    val context = new TFRuntimeContext(indices)

    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    context.enumerateTuples(new ParentKey(ParentLink()), emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num1.uri, mul.uri), t2(num2.uri, mul.uri), t2(num3.uri, add.uri))

    val newtree = Add(Mul(Num(3), Num(2)), Num(1))
    val (diffset, _) = add.compareTo(newtree)
    indices.processChangeset(diffset)

    context.enumerateTuples(new ParentKey(ParentLink()), emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num3.uri, mul.uri), t2(num2.uri, mul.uri), t2(num1.uri, add.uri))

    indices.dispose()
  }

  def isEmptyOrNull(coll: util.Collection[_]): Boolean = coll == null || coll.isEmpty

  def t1(v: Any): Tuple = Tuples.staticArityFlatTupleOf(v)

  def t2(v1: Any, v2: Any): Tuple = Tuples.staticArityFlatTupleOf(v1, v2)

  def emptyMask: TupleMask = TupleMask.identity(0)

}