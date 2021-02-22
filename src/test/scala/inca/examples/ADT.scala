package inca.examples

import inca.frontend.core._
import inca.runtime.context.LanguageMetaInfo
import truechange.{JavaLitType, SortType}

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



  val Type_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("TInt") -> SortType("Type"),
      SortType("TFun") -> SortType("Type")
    ),
    Map(
      ("TFun", "_0") -> SortType("Type"),
      ("TFun", "_1") -> SortType("Type")
    ),
    Map()
  )

  val MaybeType_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("None") -> SortType("MaybeType"),
      SortType("Some") -> SortType("MaybeType"),
    ),
    Map(
      ("Some", "_0") -> SortType("Type")
    ),
    Map()
  )

  val Ctx_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("Empty") -> SortType("Ctx"),
      SortType("Bind") -> SortType("Ctx")
    ),
    Map(
      ("Bind", "_1") -> SortType("Type"),
      ("Bind", "_2") -> SortType("Ctx")
    ),
    Map(
      ("Bind", "_0") -> JavaLitType(classOf[String])
    )
  )

  val TExp_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("TNum") -> SortType("TExp"),
      SortType("TLam") -> SortType("TExp"),
      SortType("TApp") -> SortType("TExp"),
      SortType("TVar") -> SortType("TExp"),
    ),
    Map(
      ("TLam", "_1") -> SortType("Type"),
      ("TLam", "_2") -> SortType("TExp"),
      ("TApp", "_0") -> SortType("TExp"),
      ("TApp", "_1") -> SortType("TExp"),
    ),
    Map(
      ("TNum", "_0") -> JavaLitType(classOf[Int]),
      ("TLam", "_0") -> JavaLitType(classOf[String]),
      ("TVar", "_0") -> JavaLitType(classOf[String]),
    )
  )

  val Type_code =
    s"""data Type = TInt() | TFun(Type, Type)
       |""".stripMargin
  val MaybeType_code =
    s"""data MaybeType = None() | Some(Type)
       |""".stripMargin
  val TExp_code =
    s"""data TExp = TNum(`Int`) | TLam(`String`, Type, TExp) | TApp(TExp, TExp) | TVar(`String`)
       |""".stripMargin
  val Ctx_code =
    s"""data Ctx = Empty() | Bind(`String`, Type, Ctx)
       |""".stripMargin

  val Exp_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("Num") -> SortType("Exp"),
      SortType("Lam") -> SortType("Exp"),
      SortType("App") -> SortType("Exp"),
      SortType("Var") -> SortType("Exp"),
    ),
    Map(
      ("Lam", "_1") -> SortType("Exp"),
      ("App", "_0") -> SortType("Exp"),
      ("App", "_1") -> SortType("Exp"),
    ),
    Map(
      ("Num", "_0") -> JavaLitType(classOf[Int]),
      ("Lam", "_0") -> JavaLitType(classOf[String]),
      ("Var", "_0") -> JavaLitType(classOf[String]),
    )
  )
  val Exp_code =
    s"""data Exp = Num(`Int`) | Lam(`String`, Exp) | App(Exp, Exp) | Var(`String`)
       |""".stripMargin


  val Env_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("Empty") -> SortType("Env"),
      SortType("Bind") -> SortType("Env")
    ),
    Map(
      ("Bind", "_1") -> SortType("Val"),
      ("Bind", "_2") -> SortType("Env")
    ),
    Map(
      ("Bind", "_0") -> JavaLitType(classOf[String])
    )
  )
  val Val_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("VNum") -> SortType("Val"),
      SortType("VClosure") -> SortType("Val")
    ),
    Map(
      ("VClosure", "_1") -> SortType("Exp"),
      ("VClosure", "_2") -> SortType("Env"),
    ),
    Map(
      ("VNum", "_0") -> JavaLitType(classOf[Int]),
      ("VClosure", "_0") -> JavaLitType(classOf[String]),
    )
  )

  val MaybeVal_lmi: LanguageMetaInfo = new LanguageMetaInfo(
    MultiDict(
      SortType("None") -> SortType("MaybeVal"),
      SortType("Some") -> SortType("MaybeVal"),
    ),
    Map(
      ("Some", "_0") -> SortType("Val")
    ),
    Map()
  )

  val Env_code =
    s"""data Env = Empty() | Bind(`String`, Val, Env)
       |""".stripMargin
  val Val_code =
    s"""data Val = VNum(`Int`) | VClosure(`String`, Exp, Env)
       |""".stripMargin
  val MaybeVal_code =
    s"""data MaybeVal = None() | Some(Val)
       |""".stripMargin
}
