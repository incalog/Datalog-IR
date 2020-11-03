package inca.analyzedData

import inca.frontend.core.Core.DataOp
import inca.frontend.core.Name

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

//  val NatTyp = DataType(Some(Name("inca.analyzedData.Nat")), Name("Nat"))
  val zeroOp = DataOp(Some(Name("inca.analyzedData.Nat")), Name("Zero"))
  val succOp = DataOp(Some(Name("inca.analyzedData.Nat")), Name("Succ"))

  def add(m: Nat, n: Nat): Nat = m.add(n)
  def sub(m: Nat, n: Nat): Nat = m.sub(n)

  val addOp = DataOp(Some(Name("inca.analyzedData.Nat")), Name("add"), isAssociative = true, isCommutative = true)
  val subOp = DataOp(Some(Name("inca.analyzedData.Nat")), Name("sub"), isAssociative = true)
}
