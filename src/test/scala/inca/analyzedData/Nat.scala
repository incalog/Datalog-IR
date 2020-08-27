package inca.analyzedData

import inca.frontend.core.Core.{DataOp, DataType}

object Nat {
  sealed trait Nat {
    def toInt: Int
    def add(that: Nat): Nat
    def sub(that: Nat): Nat
  }
  case object Zero extends Nat {
    override def toInt: Int = 0
    override def add(that: Nat): Nat = that
    override def sub(that: Nat): Nat = that match {
      case Zero => this
      case Succ(_) => throw new IllegalArgumentException(s"Cannot construct negative natural numbers")
    }
  }
  case class Succ(n: Nat) extends Nat {
    override def toInt: Int = n.toInt + 1
    override def add(that: Nat): Nat = Succ(n.add(that))
    override def sub(that: Nat): Nat = that match {
      case Zero => this
      case Succ(n) => this.n.sub(n)
    }
  }

  val NatTyp = DataType(Some("inca.analyzedData.Nat"), "Nat")
  val zeroOp = DataOp(Some("inca.analyzedData.Nat"), "Zero")
  val succOp = DataOp(Some("inca.analyzedData.Nat"), "Succ")

  def add(m: Nat, n: Nat): Nat = m.add(n)
  def sub(m: Nat, n: Nat): Nat = m.sub(n)

  val addOp = DataOp(Some("inca.analyzedData.Nat"), "add", isAssociative = true, isCommutative = true)
  val subOp = DataOp(Some("inca.analyzedData.Nat"), "sub", isAssociative = true)
}
