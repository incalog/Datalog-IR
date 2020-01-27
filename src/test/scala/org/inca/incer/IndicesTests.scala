package org.inca.incer

import java.util

import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, TupleMask, Tuples}
import org.inca.incer.indices.Indices
import org.inca.incer.indices.InputKey.{DataTypeKey, NodeLinkKey, NodeTypeKey}
import org.inca.meta.MetaElements.{DataType, NodeType}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.jdk.CollectionConverters._

class IndicesTests extends AnyFunSuite {

  val num1 = Num(1)
  val num2 = Num(2)
  val num3 = Num(3)
  val mul = Mul(num1, num2)
  val add = Add(mul, num3)

  test("Type hierarchy check") {
    val indices = new Indices()
    add.insert(indices)

    // superTypes
    assert(Indices.superTypeMap.get(classOf[Num]).contains(classOf[Exp]))
    assert(Indices.superTypeMap.get(classOf[Add]).contains(classOf[Exp]))
    assert(Indices.superTypeMap.get(classOf[Mul]).contains(classOf[Exp]))
    assert(isEmptyOrNull(Indices.superTypeMap.get(classOf[Exp])))

    // subTypes
    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Num])))
    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Add])))
    assert(isEmptyOrNull(Indices.subTypeMap.get(classOf[Mul])))
    assert(Indices.subTypeMap.get(classOf[Exp]).containsAll(util.Arrays.asList(classOf[Num], classOf[Add], classOf[Mul])))

    indices.dispose()
  }

  test("NodeType instances") {
    val indices = new Indices()
    add.insert(indices)

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Exp])), emptyMask, null).
      asScala should be(empty)

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Num])), emptyMask, null).
      asScala should contain allOf(t1(num1), t1(num2), t1(num3))

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Add])), emptyMask, null).
      asScala should contain(t1(add))

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Mul])), emptyMask, null).
      asScala should contain(t1(mul))

    indices.dispose()
  }

  test("DataType instances") {
    val indices = new Indices()
    add.insert(indices)

    indices.enumerateTuples(new DataTypeKey(DataType(classOf[Integer])), emptyMask, null).
      asScala should contain allOf(t1(1), t1(2), t1(3))

    indices.enumerateTuples(new DataTypeKey(DataType(classOf[String])), emptyMask, null).
      asScala should be(empty)

    indices.enumerateTuples(new DataTypeKey(DataType(classOf[Boolean])), emptyMask, null).
      asScala should be(empty)

    indices.dispose()
  }

  test("NodeLink instances") {
    val indices = new Indices()
    add.insert(indices)

    indices.enumerateTuples(new NodeLinkKey(NodeType(classOf[Num])("n")), emptyMask, null).
      asScala should contain allOf(t2(num1, 1), t2(num2, 2), t2(num3, 3))

    indices.enumerateTuples(new NodeLinkKey(NodeType(classOf[Mul])("l")), emptyMask, null).
      asScala should contain only (t2(mul, num1))

    indices.enumerateTuples(new NodeLinkKey(NodeType(classOf[Mul])("r")), emptyMask, null).
      asScala should contain only (t2(mul, num2))

    indices.enumerateTuples(new NodeLinkKey(NodeType(classOf[Add])("l")), emptyMask, null).
      asScala should contain only (t2(add, mul))

    indices.enumerateTuples(new NodeLinkKey(NodeType(classOf[Add])("r")), emptyMask, null).
      asScala should contain only (t2(add, num3))
  }

  def isEmptyOrNull(coll: util.Collection[_]): Boolean = coll == null || coll.isEmpty

  def t1(v: Any): Tuple = Tuples.staticArityFlatTupleOf(v)

  def t2(v1: Any, v2: Any): Tuple = Tuples.staticArityFlatTupleOf(v1, v2)

  def emptyMask: TupleMask = TupleMask.identity(0)

}