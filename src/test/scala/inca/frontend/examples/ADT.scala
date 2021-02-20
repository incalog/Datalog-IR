package inca.frontend.examples

import inca.frontend.core._
import inca.runtime.context.LanguageMetaInfo
import truechange.SortType

import scala.collection.immutable.MultiDict

object ADT {

  val NAT_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("Zero") -> SortType("Nat"),
      SortType("Succ") -> SortType("Nat")),
    Map(
      ("Succ", "_0") -> SortType("Nat")
    ),
    Map()
  )

  val Nat_code =
    s"""data Nat = Zero() | Succ(Nat)
       |""".stripMargin
  val Zero = DataConstructor(Name("Zero"), Seq())
  val Succ = DataConstructor(Name("Succ"), Seq(TData(Name("Nat"))))
  val Nat = DataDef(Seq(), None, Name("Nat"), Seq(Zero, Succ))

  val TNat = TData(Name("Nat"))
}
