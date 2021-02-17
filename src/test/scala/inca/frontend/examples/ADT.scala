package inca.frontend.examples

import inca.frontend.core._

object ADT {

  val Zero = DataConstructor(Name("Zero"), Seq())
  val Succ = DataConstructor(Name("Succ"), Seq(TData(Name("Nat"))))
  val Nat = DataDef(None, Name("Nat"), Seq(Zero, Succ))

  def CallZero = Call(Name("Zero"), Seq()).resolved(Zero)
  def CallSucc(exp: Expression) = Call(Name("Succ"), Seq(exp)).resolved(Succ)
  val TNat = TData(Name("Nat"))

}
