package inca.backend

import java.util
import java.util.Collections

import inca.MetaElements.{DataType, NodeType}
import inca.backend.indices.TFInputKey.{DataTypeKey, NodeLinkKey, NodeTypeKey}
import inca.backend.indices.{Indices, TFRuntimeContext}
import inca.backend.virtual.{ParentIndex, ParentKey, VirtualIndex}
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, TupleMask, Tuples}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truediff.Diffable

import scala.jdk.CollectionConverters._

class RuntimeContextTests extends AnyFunSuite {

  val num1 = Num(1)
  val num2 = Num(2)
  val num3 = Num(3)
  val mul = Mul(num1, num2)
  val add = Add(mul, num3)

  val expName = classOf[Exp].getCanonicalName
  val addName = classOf[Add].getCanonicalName
  val mulName = classOf[Mul].getCanonicalName
  val numName = classOf[Num].getCanonicalName

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

    val context = new TFRuntimeContext(indices, null)

    context.enumerateTuples(new NodeTypeKey(NodeType(expName)), emptyMask, null).
      asScala should be(empty)

    context.enumerateTuples(new NodeTypeKey(NodeType(numName)), emptyMask, null).
      asScala should contain allOf(t1(num1.uri), t1(num2.uri), t1(num3.uri))

    context.enumerateTuples(new NodeTypeKey(NodeType(addName)), emptyMask, null).
      asScala should contain(t1(add.uri))

    context.enumerateTuples(new NodeTypeKey(NodeType(mulName)), emptyMask, null).
      asScala should contain(t1(mul.uri))

    indices.dispose()
  }

  test("DataType instances") {
    val indices = new Indices()
    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    val integerName = "java.lang.Integer"
    val stringName = "java.lang.String"
    val boolName = "java.lang.Boolean"

    val context = new TFRuntimeContext(indices, null)

    context.enumerateTuples(new DataTypeKey(DataType(integerName)), emptyMask, null).
      asScala should contain allOf(t1(1), t1(2), t1(3))

    context.enumerateTuples(new DataTypeKey(DataType(stringName)), emptyMask, null).
      asScala should be(empty)

    context.enumerateTuples(new DataTypeKey(DataType(boolName)), emptyMask, null).
      asScala should be(empty)

    indices.dispose()
  }

  test("NodeLink instances") {
    val indices = new Indices()
    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    val context = new TFRuntimeContext(indices, null)

    context.enumerateTuples(new NodeLinkKey(NodeType(numName)("n")), emptyMask, null).
      asScala should contain allOf(t2(num1.uri, 1), t2(num2.uri, 2), t2(num3.uri, 3))

    context.enumerateTuples(new NodeLinkKey(NodeType(mulName)("l")), emptyMask, null).
      asScala should contain only (t2(mul.uri, num1.uri))

    context.enumerateTuples(new NodeLinkKey(NodeType(mulName)("r")), emptyMask, null).
      asScala should contain only (t2(mul.uri, num2.uri))

    context.enumerateTuples(new NodeLinkKey(NodeType(addName)("l")), emptyMask, null).
      asScala should contain only (t2(add.uri, mul.uri))

    context.enumerateTuples(new NodeLinkKey(NodeType(addName)("r")), emptyMask, null).
      asScala should contain only (t2(add.uri, num3.uri))

    indices.dispose()
  }

  test("VirtualLink parent") {
    val virtualIndices = new java.util.HashMap[String, VirtualIndex]()
    virtualIndices.put(ParentKey.getUniqueID, new ParentIndex())
    val indices = new Indices(null, null, null, virtualIndices)
    val context = new TFRuntimeContext(indices, null)

    val changeset = Diffable.load(add)
    indices.processChangeset(changeset)

    context.enumerateTuples(ParentKey, emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num1.uri, mul.uri), t2(num2.uri, mul.uri), t2(num3.uri, add.uri))

    val newtree = Add(Mul(Num(3), Num(2)), Num(1))
    val (diffset, _) = add.compareTo(newtree)
    indices.processChangeset(diffset)

    context.enumerateTuples(ParentKey, emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num3.uri, mul.uri), t2(num2.uri, mul.uri), t2(num1.uri, add.uri))

    indices.dispose()
  }

  def isEmptyOrNull(coll: util.Collection[_]): Boolean = coll == null || coll.isEmpty

  def t1(v: Any): Tuple = Tuples.staticArityFlatTupleOf(v)

  def t2(v1: Any, v2: Any): Tuple = Tuples.staticArityFlatTupleOf(v1, v2)

  def emptyMask: TupleMask = TupleMask.identity(0)

}