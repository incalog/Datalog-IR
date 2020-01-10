package org.inca.incer

import org.inca.incer.indices.Indices
import org.inca.meta.MetaElements.{DataType, NodeType}


@IncrementalIndex
trait ITest {

}
@IncrementalIndex
case class Foo() extends ITest
@IncrementalIndex
case class Test(val n : Int, val o : Option[Incrementalizable], val t : Incrementalizable, val s : Seq[Incrementalizable]) extends ITest{

}

trait Exp extends Incrementalizable {
  override def insert(indices: Indices): Unit = {
    super.insert(indices)
    // NodeType instances
    indices.insertNodeTypeInstance(NodeType(classOf[Exp]), this)
  }
}
object Exp {
  Indices.registerType(classOf[Exp], null)
}

object ExpTypes extends Incrementalizable {
  override def insert(indices: Indices): Unit = {
    indices.insertType(classOf[Exp])
    indices.insertType(classOf[Num])
    indices.insertType(classOf[BinaryExpression])
    indices.insertType(classOf[Add])
    indices.insertType(classOf[Mul])
  }

  override def delete(indices: Indices): Unit = {}
}

// 1. report the relevant types: (sub,sup)
// 2. build up the sub - sup type index

case class Num(n: Int) extends Exp {
  override def insert(indices: Indices): Unit = {
    super.insert(indices)

    // NodeType instances
    indices.insertNodeTypeInstance(NodeType(classOf[Num]), this)

    // NodeLink instances
    indices.insertNodeLinkInstance(this, NodeType(classOf[Num])("n"), this.n)

    // DataType instances
    indices.insertDataTypeInstance(DataType(classOf[Int]), this.n)
  }

  override def delete(indices: Indices): Unit = {
    // NodeType instances
    indices.deleteNodeTypeInstance(NodeType(classOf[Exp]), this)
    indices.deleteNodeTypeInstance(NodeType(classOf[Num]), this)

    // NodeLink instances
    indices.deleteNodeLinkInstance(this, NodeType(classOf[Num])("n"), this.n)

    // DataType instances
    indices.deleteDataTypeInstance(DataType(classOf[Int]), this.n)
  }
}
object Num {
  Indices.registerType(classOf[Num], classOf[Exp])
}

abstract class BinaryExpression(val lhs: Exp, val rhs: Exp) extends Exp {

}

case class Add(l: Exp, r: Exp) extends BinaryExpression(l, r) {
  override def insert(indices: Indices): Unit = {
    super.insert(indices)
    // NodeType instances
    indices.insertNodeTypeInstance(NodeType(classOf[Add]), this)

    // NodeLink instances
    indices.insertNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("lhs"), lhs)
    indices.insertNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("rhs"), rhs)

    // recursively build indices for children
    lhs.insert(indices)
    rhs.insert(indices)
  }

  override def delete(indices: Indices): Unit = {
    // NodeType instances
    indices.deleteNodeTypeInstance(NodeType(classOf[Add]), this)

    // NodeLink instances
    indices.deleteNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("lhs"), lhs)
    indices.deleteNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("rhs"), rhs)

    // recursively build indices for children
    lhs.insert(indices)
    rhs.insert(indices)
  }
}

case class Mul(e1: Exp, e2: Exp) extends BinaryExpression(e1, e2) {

  override def insert(indices: Indices): Unit = {
    // NodeType instances
    indices.insertNodeTypeInstance(NodeType(classOf[Mul]), this)

    // NodeLink instances
    indices.insertNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("lhs"), lhs)
    indices.insertNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("rhs"), rhs)

    // recursively build indices for children
    lhs.insert(indices)
    rhs.insert(indices)
  }

  override def delete(indices: Indices): Unit = {
    // NodeType instances
    indices.deleteNodeTypeInstance(NodeType(classOf[Mul]), this)

    // NodeLink instances
    indices.deleteNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("lhs"), lhs)
    indices.deleteNodeLinkInstance(this, NodeType(classOf[BinaryExpression])("rhs"), rhs)

    // recursively build indices for children
    lhs.insert(indices)
    rhs.insert(indices)
  }

}
