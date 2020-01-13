package org.inca.incer

import java.util

import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, TupleMask, Tuples}
import org.inca.incer.indices.Indices
import org.inca.incer.indices.InputKey.NodeTypeKey
import org.inca.meta.MetaElements.NodeType
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

import scala.jdk.CollectionConverters._

class ExpressionTestSuite extends AnyFunSuite {

  test("Type hierarchy check") {
    val exp = Add(Mul(Num(1), Num(2)), Num(3))
    val indices = new Indices()
    exp.insert(indices)

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
  }

  test("NodeType instances") {
    val n1 = Num(1)
    val n2 = Num(2)
    val n3 = Num(3)
    val m = Mul(n1, n2)
    val a = Add(m, n3)
    val indices = new Indices()
    a.insert(indices)

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Exp])), emptyMask, null).
      asScala should contain allOf(t(n1), t(n2), t(n3), t(m), t(a))

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Num])), emptyMask, null).
      asScala should contain allOf(t(n1), t(n2), t(n3))

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Add])), emptyMask, null).
      asScala should contain(t(a))

    indices.enumerateTuples(new NodeTypeKey(NodeType(classOf[Mul])), emptyMask, null).
      asScala should contain(t(m))
  }

  def isEmptyOrNull(coll: util.Collection[_]): Boolean = coll == null || coll.isEmpty

  def t(v: Any): Tuple = Tuples.staticArityFlatTupleOf(v)

  def emptyMask: TupleMask = TupleMask.identity(0)

}