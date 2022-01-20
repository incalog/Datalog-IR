package inca.runtime

import java.{lang, util}
import inca.analyzedLangs.Exp._
import inca.analyzedLangs.{Exp, tinyJava}
import inca.runtime.db.Database
import inca.runtime.index._
import inca.runtime.index.dynamic.ParentIndex
import inca.runtime.index.virtual.SizeIndex
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, TupleMask, Tuples}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
import truechange.{JavaLitType, ListType, SortType}
import truediff.Diffable

import scala.jdk.CollectionConverters._


class RuntimeContextTests extends AnyFunSuite {

  val num1 = IntegerLit(1)
  val num2 = IntegerLit(2)
  val num3 = IntegerLit(3)
  val mul = Mul(num1, num2)
  val add = Add(mul, num3)

  val expName = classOf[Exp].getCanonicalName
  val addName = classOf[Add].getCanonicalName
  val mulName = classOf[Mul].getCanonicalName
  val numName = classOf[IntegerLit].getCanonicalName

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

    val (updateScript, newtree) = add.compareTo(Add(Mul(IntegerLit(4), IntegerLit(5)), IntegerLit(6)))
    database.processEditScript(updateScript)

    database.enumerateTuples(PrimitiveTypeKey(integer), emptyMask, null).
      asScala should contain allOf(t1(4), t1(5), t1(6))

    database.enumerateTuples(PrimitiveTypeKey(string), emptyMask, null).
      asScala should be(empty)

    database.enumerateTuples(PrimitiveTypeKey(bool), emptyMask, null).
      asScala should be(empty)
  }

  test("DataType instances bag semantics") {
    val database = new Database()
    val editScript = Diffable.load(Add(Mul(IntegerLit(1), IntegerLit(1)), IntegerLit(1)))
    database.processEditScript(editScript)

    val integer = JavaLitType(classOf[lang.Integer])
    val string = JavaLitType(classOf[lang.String])
    val bool = JavaLitType(classOf[lang.Boolean])

    database.enumerateTuples(PrimitiveTypeKey(integer), emptyMask, null).
      asScala should contain (t1(1))

    database.enumerateTuples(PrimitiveTypeKey(string), emptyMask, null).
      asScala should be(empty)

    database.enumerateTuples(PrimitiveTypeKey(bool), emptyMask, null).
      asScala should be(empty)

    val (updateScript, newtree) = add.compareTo(Add(Mul(IntegerLit(1), IntegerLit(5)), IntegerLit(6)))
    database.processEditScript(updateScript)

    database.enumerateTuples(PrimitiveTypeKey(integer), emptyMask, null).
      asScala should contain allOf(t1(1), t1(5), t1(6))

    database.enumerateTuples(PrimitiveTypeKey(string), emptyMask, null).
      asScala should be(empty)

    database.enumerateTuples(PrimitiveTypeKey(bool), emptyMask, null).
      asScala should be(empty)
  }

  test("NodeLink instances") {
    val database = new Database()
    val editScript = Diffable.load(add)
    database.processEditScript(editScript)

    database.enumerateTuples(LinkPrimitiveKey(numName->"value"), emptyMask, null).
      asScala should contain allOf(t2(num1.uri, 1), t2(num2.uri, 2), t2(num3.uri, 3))

    database.enumerateTuples(LinkNodeKey(mulName->"lhs"), emptyMask, null).
      asScala should contain only (t2(mul.uri, num1.uri))

    database.enumerateTuples(LinkNodeKey(mulName->"rhs"), emptyMask, null).
      asScala should contain only (t2(mul.uri, num2.uri))

    database.enumerateTuples(LinkNodeKey(addName->"lhs"), emptyMask, null).
      asScala should contain only (t2(add.uri, mul.uri))

    database.enumerateTuples(LinkNodeKey(addName->"rhs"), emptyMask, null).
      asScala should contain only (t2(add.uri, num3.uri))

  }

  test("VirtualLink parent") {
    val additionalIndices = Seq(ParentIndex.Factory)
    val database = new Database(null, additionalIndices, null)

    val editScript = Diffable.load(add)
    database.processEditScript(editScript)

    database.enumerateTuples(ParentIndex.Key, emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num1.uri, mul.uri), t2(num2.uri, mul.uri), t2(num3.uri, add.uri))

    val newtree = Add(Mul(IntegerLit(3), IntegerLit(2)), IntegerLit(1))
    val (diffset, _) = add.compareTo(newtree)
    database.processEditScript(diffset)

    database.enumerateTuples(ParentIndex.Key, emptyMask, null).
      asScala should contain allOf(t2(mul.uri, add.uri), t2(num1.uri, mul.uri), t2(num2.uri, mul.uri), t2(num3.uri, add.uri))
  }

  test("List children") {
    val additionalIndices = Seq(ParentIndex.Factory)
    val database = new Database(null, additionalIndices, null)

    val exp = Many(List(num1, num2))
    val li = exp.exps

    val editScript = Diffable.load(exp)
    database.processEditScript(editScript)

    database.enumerateTuples(ParentIndex.Key, emptyMask, null).
      asScala should contain allOf(t2(li.uri, exp.uri), t2(num1.uri, li.uri), t2(num2.uri, li.uri))

    database.enumerateTuples(SizeIndex.Key, emptyMask, null).
      asScala should contain (t2(li.uri, 2))

    database.enumerateTuples(NodeTypeKey(ListType(SortType(expTag))), emptyMask, null).
      asScala should contain (t1(li.uri))

    val newtree = Many(List(IntegerLit(3), IntegerLit(1)))
    val (diffset, updatedTree) = exp.compareTo(newtree)
    database.processEditScript(diffset)

    database.enumerateTuples(ParentIndex.Key, emptyMask, null).
      asScala should contain allOf(t2(li.uri, exp.uri), t2(updatedTree.exps(1).uri, li.uri), t2(updatedTree.exps(0).uri, li.uri))

    database.enumerateTuples(SizeIndex.Key, emptyMask, null).
      asScala should contain (t2(li.uri, 2))

    database.enumerateTuples(NodeTypeKey(ListType(SortType(expTag))), emptyMask, null).
      asScala should contain (t1(li.uri))

  }

  test("firstlink and nextlink of list") {
    val additionalIndices = Seq(ParentIndex.Factory)
    val database = new Database(null, additionalIndices, null)

    import tinyJava._
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