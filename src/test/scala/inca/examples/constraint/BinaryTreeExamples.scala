package inca.examples.constraint

import inca.frontend.constraint.executor.ConstraintExecutor
import inca.runtime.context.DataModel
import org.scalatest.funsuite.AnyFunSuite
import truediff.Diffable
import truediff.macros.diffable

// language definition (data model as case classes)
// IMPORTANT needs to be a top-level definition
@diffable trait Tree extends Diffable
@diffable case class BinaryNode(v: Int, l: Tree, r: Tree) extends Tree
@diffable case class LeafNode() extends Tree
object BinaryTreeModel {
  // meta information about data model
  val model: DataModel =
    DataModel.from(Tree, BinaryNode, LeafNode)
}

class BinaryTreeExamples extends AnyFunSuite {


  test("different functions for binary trees") {
    val code =
     s"""module BinaryTreeAnalyses
        |datamodel inca.examples.constraint.BinaryTreeModel.model
        |
        |node inca.examples.constraint._
        |
        |@main
        |def rootNode(t: Tree): Unit = {
        |  assert undef t.parent
        |}
        |
        |def lhs(t: Tree): Tree = {
        |  val binary = t:BinaryNode
        |  yield binary.l
        |} union {
        |  assert t.isInstanceOf[LeafNode]
        |  fail
        |}
        |
        |def emptyBinaryNode(t: BinaryNode): Unit = {
        |  assert undef t.l
        |  assert undef t.r
        |}
        |""".stripMargin

    val loaded = ConstraintExecutor.loadAnalysis(code)

    val tree = BinaryNode(4, BinaryNode(2, LeafNode(), LeafNode()), LeafNode())
//    println(tree.toStringWithURI)
    val tree2 = BinaryNode(4, BinaryNode(2, BinaryNode(1, LeafNode(), LeafNode()), LeafNode()), LeafNode())
//    println(tree2.toStringWithURI)
    val res1 = loaded.execute(tree, "rootNode")
//    res1.foreach(println)
    val res2 = loaded.update(tree2, "rootNode")
//    res2.foreach(println)
  }
}
