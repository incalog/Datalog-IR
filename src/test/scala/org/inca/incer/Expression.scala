package org.inca.incer

import org.inca.incer.indices.Indices
import org.inca.meta.MetaElements.{DataType, NodeLink, NodeType}

@IncrementalIndex
trait Exp extends Incrementalizable

case class Num(n: Int) extends Exp {
  override def insert(indices: Indices): Unit = {
    indices.insertNodeTypeInstance(NodeType(this.getClass), this)
    indices.insertDataTypeInstance(DataType(Int.getClass), this.n)
  }

  override def delete(indices: Indices): Unit = {
    indices.deleteNodeTypeInstance(NodeType(this.getClass), this)
    indices.deleteDataTypeInstance(DataType(Int.getClass), this.n)
  }
}

abstract class BinaryExpression(lhs: Exp, rhs: Exp) extends Exp {
  override def insert(indices: Indices): Unit = {
    indices.insertNodeTypeInstance(NodeType(this.getClass), this)
    indices.insertNodeLinkInstance(this, NodeType(this.getClass)("lhs"), lhs)
    indices.insertNodeLinkInstance(this, NodeType(this.getClass)("rhs"), lhs)
    lhs.insert(indices)
    rhs.insert(indices)
  }

  override def delete(indices: Indices): Unit = {
    indices.insertNodeTypeInstance(NodeType(this.getClass), this)
    indices.deleteNodeLinkInstance(this, NodeType(this.getClass)("lhs"), lhs)
    indices.deleteNodeLinkInstance(this, NodeType(this.getClass)("rhs"), lhs)
    lhs.delete(indices)
    rhs.delete(indices)
  }
}

case class Add(e1: Exp, e2: Exp) extends BinaryExpression(e1, e2) {}

case class Mul(e1: Exp, e2: Exp) extends BinaryExpression(e1, e2) {}
