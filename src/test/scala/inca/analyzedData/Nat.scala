package inca.analyzedData

import inca.frontend.constraint.core.tree._
import inca.runtime.aggregate.Aggregation
import inca.util.Meta
import inca.util.Meta.Scala

import scala.meta._

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

  val NatTyp = TScala("inca.analyzedData.Nat.Nat")
  val zeroOp = Scala(q"inca.analyzedData.Nat.Zero")
  val succOp = Scala(q"inca.analyzedData.Nat.Succ")

  val sumAgg = new Aggregation[Nat] {
    override val name: String = "sum"
    override def init: Nat = Zero
    override def join(v1: Nat, v2: Nat): Nat = v1.add(v2)
    override val isAssociative: Boolean = true
    override val isCommutative: Boolean = true
  }
  val sumAggregation = Scala(Meta.mkQualName("inca.analyzedData.Nat.sumAgg"))

  val fastSumAggregation = Scala(
    q"""{import inca.analyzedData.Nat.{Nat, Zero, Succ}
        new inca.runtime.aggregate.Aggregation[Nat] {
          override val name: String = "sum"
          override def init: Nat = Zero
          override def join(v1: Nat, v2: Nat): Nat = v1.add(v2)
          override def unjoin(v1: Nat, v2: Nat): Nat = v1.sub(v2)
          override val isAssociative: Boolean = true
          override val isCommutative: Boolean = true
          override val hasUnjoin: Boolean = true
        }}
      """)
}
