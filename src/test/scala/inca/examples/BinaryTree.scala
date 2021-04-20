package inca.examples

import inca.Executor
import inca.runtime.context.LanguageMetaInfo
import org.scalatest.funsuite.AnyFunSuite
import truechange.{JavaLitType, SortType}
import truediff.macros.diffable

import scala.collection.immutable.MultiDict


class BinaryTree extends AnyFunSuite {

  // language definition (data model as case classes)
  @diffable
  trait Tree
  @diffable
  case class BinaryNode(v: Int, l: Tree, r: Tree) extends Tree
  @diffable
  case class LeafNode() extends Tree
  val treeTag = classOf[Tree].getCanonicalName
  val binaryTag = classOf[BinaryNode].getCanonicalName
  val leafTag = classOf[LeafNode].getCanonicalName

  // meta information about data model
  val languageMetaInfo: LanguageMetaInfo = {
    val treeType = SortType(treeTag)
    val binaryType = SortType(binaryTag)
    val leafType = SortType(leafTag)
    new LanguageMetaInfo(
      MultiDict[SortType, SortType](
        binaryType -> treeType,
        leafType -> treeType,
      ),
      Map(
        (binaryTag->"l") -> treeType,
        (binaryTag->"r") -> treeType,
      ),
      Map(
        (binaryTag->"v") -> JavaLitType(classOf[java.lang.Integer]),
      )
    )
  }

  test("different functions for binary trees") {
    val code =
     s"""module Test
        |
        |def rootNode(t: $treeTag): Unit = {
        |  assert undef t.parent
        |}
        |
        |def lhs(t: $treeTag): $treeTag = {
        |  val binary = t:$binaryTag
        |  yield binary.l
        |} union {
        |  assert t.isInstanceOf[$leafTag]
        |  fail
        |}
        |
        |def emptyBinaryNode(t: $binaryTag): Unit = {
        |  assert undef t.l
        |  assert undef t.r
        |}
        |""".stripMargin
      ""

    val loaded = Executor.loadAnalysis(code, languageMetaInfo)

    val tree = BinaryNode(4, BinaryNode(2, LeafNode(), LeafNode()), LeafNode())
    val tree2 = BinaryNode(4, BinaryNode(2, BinaryNode(1, LeafNode(), LeafNode()), LeafNode()), LeafNode())
    val res1 = loaded.execute(tree, "rootNode")
    res1.foreach(println)
    val res2 = loaded.update(tree2, "rootNode")
    res2.foreach(println)
  }
}
