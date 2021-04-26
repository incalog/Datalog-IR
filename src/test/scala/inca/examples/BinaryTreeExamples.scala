package inca.examples

import inca.Executor
import inca.runtime.context.LanguageMetaInfo
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable
import truediff.macros.diffable

// language definition (data model as case classes)
// IMPORTANT needs to be a top-level definition
@diffable trait Tree extends Diffable
@diffable case class BinaryNode(v: Int, l: Tree, r: Tree) extends Tree
@diffable case class LeafNode() extends Tree

class BinaryTreeExamples extends AnyFunSuite {

  // meta information about data model
  val languageMetaInfo: LanguageMetaInfo =
    LanguageMetaInfo.from(Tree, BinaryNode, LeafNode)

  test("different functions for binary trees") {
    val code =
     s"""module BinaryTreeAnalyses
        |
        |def rootNode(t: inca.examples.Tree): Unit = {
        |  assert undef t.parent
        |}
        |
        |def lhs(t: inca.examples.Tree): inca.examples.Tree = {
        |  val binary = t:inca.examples.BinaryNode
        |  yield binary.l
        |} union {
        |  assert t.isInstanceOf[inca.examples.LeafNode]
        |  fail
        |}
        |
        |def emptyBinaryNode(t: inca.examples.BinaryNode): Unit = {
        |  assert undef t.l
        |  assert undef t.r
        |}
        |""".stripMargin

    val loaded = Executor.loadAnalysis(code, languageMetaInfo)

    val tree = BinaryNode(4, BinaryNode(2, LeafNode(), LeafNode()), LeafNode())
    println(tree.toStringWithURI)
    val tree2 = BinaryNode(4, BinaryNode(2, BinaryNode(1, LeafNode(), LeafNode()), LeafNode()), LeafNode())
    println(tree2.toStringWithURI)
    val res1 = loaded.execute(tree, "rootNode")
    res1.foreach(println)
    val res2 = loaded.update(tree2, "rootNode")
    res2.foreach(println)
  }
}
