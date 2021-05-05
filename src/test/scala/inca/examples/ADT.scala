package inca.examples

import inca.frontend.functional.core._
import inca.runtime.context.LanguageMetaInfo
import truechange.SortType

import scala.collection.immutable.MultiDict

object ADT {

  val Nat_lmi: LanguageMetaInfo = new LanguageMetaInfo(
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


  val Type_code =
    s"""data Type = TInt() | TFun(Type, Type)
       |""".stripMargin
  val MaybeType_code =
    s"""data MaybeType = NoType() | SomeType(Type)
       |""".stripMargin
  val TExp_code =
    s"""data TExp = TNum(Int) | TLam(String, Type, TExp) | TApp(TExp, TExp) | TVar(String)
       |""".stripMargin
  val Ctx_code =
    s"""data Ctx = EmptyCtx() | BindCtx(String, Type, Ctx)
       |""".stripMargin

  val Exp_code =
    s"""data Exp = Num(Int) | Lam(String, Exp) | App(Exp, Exp) | Var(String)
       |""".stripMargin

  val Env_code =
    s"""data Env = EmptyEnv() | BindEnv(String, Val, Env)
       |""".stripMargin
  val Val_code =
    s"""data Val = VNum(Int) | VClosure(String, Exp, Env)
       |""".stripMargin
  val MaybeVal_code =
    s"""data MaybeVal = NoVal() | SomeVal(Val)
       |""".stripMargin
}
