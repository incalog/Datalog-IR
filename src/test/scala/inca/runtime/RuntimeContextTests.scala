package inca.runtime

import java.{lang, util}

import inca.analyzedLangs._
import inca.runtime.index._
import inca.runtime.index.dynamic.{DynamicIndex, ParentIndex}
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, TupleMask, Tuples}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truechange.{JavaLitType, ListType, SortType}
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
    val database = new Database()
    val editScript = Diffable.load(add)
    database.processEditScript(editScript)

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

  }

  test("NodeType instances") {
    val database = new Database()
    val editScript = Diffable.load(add)
    database.processEditScript(editScript)

    database.enumerateTuples(NodeTypeKey(SortType(expName)), emptyMask, null).
      asScala should be(empty)

    database.enumerateTuples(new NodeTypeKey(SortType(numName)), emptyMask, null).
      asScala should contain allOf(t1(num1.uri), t1(num2.uri), t1(num3.uri))

    database.enumerateTuples(new NodeTypeKey(SortType(addName)), emptyMask, null).
      asScala should contain(t1(add.uri))

    database.enumerateTuples(new NodeTypeKey(SortType(mulName)), emptyMask, null).
      asScala should contain(t1(mul.uri))

  }

  test("DataType instances") {
    val database = new Database()
    val editScript = Diffable.load(add)
    database.processEditScript(editScript)

    val integer = JavaLitType(classOf[lang.Integer])
    val string = JavaLitType(classOf[lang.String])
    val bool = JavaLitType(classOf[lang.Boolean])

    database.enumerateTuples(PrimitiveTypeKey(integer), emptyMask, null).
      asScala should contain allOf(t1(1), t1(2), t1(3))

    database.enumerateTuples(PrimitiveTypeKey(string), emptyMask, null).
      asScala should be(empty)

    database.enumerateTuples(PrimitiveTypeKey(bool), emptyMask, null).
      asScala should be(empty)

  }

  test("NodeLink instances") {
    val database = new Database()
    val editScript = Diffable.load(add)
    database.processEditScript(editScript)

    database.enumerateTuples(LinkPrimitiveKey(numName->"n"), emptyMask, null).
      asScala should contain allOf(t2(num1.uri, 1), t2(num2.uri, 2), t2(num3.uri, 3))

    database.enumerateTuples(LinkNodeKey(mulName->"l"), emptyMask, null).
      asScala should contain only (t2(mul.uri, num1.uri))

    database.enumerateTuples(LinkNodeKey(mulName->"r"), emptyMask, null).
      asScala should contain only (t2(mul.uri, num2.uri))

    database.enumerateTuples(LinkNodeKey(addName->"l"), emptyMask, null).
      asScala should contain only (t2(add.uri, mul.uri))

    database.enumerateTuples(LinkNodeKey(addName->"r"), emptyMask, null).
      asScala should contain only (t2(add.uri, num3.uri))

  }

  test("VirtualLink parent") {
    val dynamicIndices = Map[DynamicKey, DynamicIndex](ParentIndex())
    val database = new Database(null, dynamicIndices, null)

    val editScript = Diffable.load(add)
    database.processEditScript(editScript)

    database.enumerateTuples(ParentIndex.Key, emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num1.uri, mul.uri), t2(num2.uri, mul.uri), t2(num3.uri, add.uri))

    val newtree = Add(Mul(Num(3), Num(2)), Num(1))
    val (diffset, _) = add.compareTo(newtree)
    database.processEditScript(diffset)

    database.enumerateTuples(ParentIndex.Key, emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num3.uri, mul.uri), t2(num2.uri, mul.uri), t2(num1.uri, add.uri))
  }

  test("firstlink and nextlink of list") {
    val dynamicIndices = Map[DynamicKey, DynamicIndex](ParentIndex())
    val database = new Database(null, dynamicIndices, null)

    val classDeclTag = classOf[ClassDeclaration].getCanonicalName
    val classDeclType = SortType(classDeclTag)
    val classMemberType = SortType(classOf[ClassMember].getCanonicalName)
    val fieldDecl1 = FieldDeclaration("bar", PublicVisibility())
    val fieldDecl2 = FieldDeclaration("baz", PrivateVisibility())
    val clazz = ClassDeclaration("Foo", true, List(fieldDecl1, fieldDecl2))

    val editScript = Diffable.load(clazz)
    database.processEditScript(editScript)

    database.enumerateTuples(LinkNodeKey(classDeclTag->"members"), emptyMask, null).
      asScala should contain only t2(clazz.uri, clazz.members.uri)

    database.enumerateTuples(NodeTypeKey(ListType(classMemberType)), emptyMask, null).
      asScala should contain only t1(clazz.members.uri)

    database.enumerateTuples(ParentIndex.Key, secondElementMask, t2(null, clazz.members.uri)).
      asScala should contain only (t2(fieldDecl1.uri, clazz.members.uri), t2(fieldDecl2.uri, clazz.members.uri))

    database.enumerateTuples(LinkListNextKey, emptyMask, null).
      asScala should contain only t2(fieldDecl1.uri, fieldDecl2.uri)

    val fieldDecl3 = FieldDeclaration("baaz", PublicVisibility())
    val clazz2 = ClassDeclaration("Foo", true, List(fieldDecl2, fieldDecl1, fieldDecl3))
    val (diffScript, updatedclazz) = clazz.compareTo(clazz2)
    database.processEditScript(diffScript)

    database.enumerateTuples(LinkNodeKey(classDeclTag->"members"), emptyMask, null).
      asScala should contain only t2(updatedclazz.uri, updatedclazz.members.uri)

    database.enumerateTuples(NodeTypeKey(ListType(classMemberType)), emptyMask, null).
      asScala should contain only t1(updatedclazz.members.uri)

    database.enumerateTuples(ParentIndex.Key, secondElementMask, t2(null, updatedclazz.members.uri)).
      asScala should contain only (
        t2(updatedclazz.members(0).uri, updatedclazz.members.uri),
        t2(updatedclazz.members(1).uri, updatedclazz.members.uri),
        t2(updatedclazz.members(2).uri, updatedclazz.members.uri)
      )

    database.enumerateTuples(LinkListNextKey, emptyMask, null).
      asScala should contain only (t2(updatedclazz.members(0).uri, updatedclazz.members(1).uri), t2(updatedclazz.members(1).uri, updatedclazz.members(2).uri))

  }


  def isEmptyOrNull(coll: util.Collection[_]): Boolean = coll == null || coll.isEmpty

  def t1(v: Any): Tuple = Tuples.staticArityFlatTupleOf(v)

  def t2(v1: Any, v2: Any): Tuple = Tuples.staticArityFlatTupleOf(v1, v2)

  def emptyMask: TupleMask = TupleMask.identity(0)

  def firstElementMask: TupleMask = TupleMask.selectSingle(0, 2)
  def secondElementMask: TupleMask = TupleMask.selectSingle(1, 2)

}