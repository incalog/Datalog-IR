package inca.frontend.examples

import inca.frontend.core._
import inca.frontend.examples.ADT._

import scala.meta.XtensionQuasiquoteTerm

object AST {
  def module(content: ModuleContent*): Module =
    Module(Name("Main"), Seq(), content)

  val baseExample: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    BaseLit(q"7 + (12 * 3)", TScalaInt)
  ).addAnnotation(MainFunctionAnno))
  val baseExample2: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    BaseApplyInfix(BaseLit(q"7", TScalaInt), "+",
      BaseApplyInfix(BaseLit(q"12", TScalaInt), "*", BaseLit(q"3", TScalaInt), TScalaInt),
      TScalaInt
    )
  ).addAnnotation(MainFunctionAnno))

  val varExample: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, BaseLit(q"7", TScalaInt),
      Let(Seq(Name("y")), None, BaseLit(q"3", TScalaInt),
        BaseApplyInfix(Var("x").typed(TScalaInt), "+",
          BaseApplyInfix(BaseLit(q"12", TScalaInt), "*", Var("y").typed(TScalaInt), TScalaInt),
          TScalaInt
        )
      )
    )
  ).addAnnotation(MainFunctionAnno))

  val ifExample: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, BaseLit(q"7", TScalaInt),
      If(BaseApplyInfix(Var("x").typed(TScalaInt), ">", BaseLit(q"0", TScalaInt), TScalaBoolean),
        Var("x"),
        BaseApplyInfix(Var("x").typed(TScalaInt), "*", BaseLit(q"-1", TScalaInt), TScalaInt)
      )
    )
  ).addAnnotation(MainFunctionAnno))

  val ifExample2: Module = module(FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, BaseLit(q"7", TScalaInt),
      Let(Seq(Name("y")), None, BaseLit(q"-3", TScalaInt),
        BaseApplyInfix(
          If(BaseApplyInfix(Var("x").typed(TScalaInt), ">", BaseLit(q"0", TScalaInt), TScalaBoolean),
            Var("x"),
            BaseApplyInfix(Var("x").typed(TScalaInt), "*", BaseLit(q"-1", TScalaInt), TScalaInt)
          ).typed(TScalaInt),
          "+",
          If(BaseApplyInfix(Var("y").typed(TScalaInt), ">", BaseLit(q"0", TScalaInt), TScalaBoolean),
            Var("y"),
            BaseApplyInfix(Var("y").typed(TScalaInt), "*", BaseLit(q"-1", TScalaInt), TScalaInt)
          ).typed(TScalaInt),
          TScalaInt
        )
      )
    )
  ).addAnnotation(MainFunctionAnno))


  val incFun: FunctionDef = FunctionDef(None, Name("inc"), Seq(Param(Name("n"), TScalaInt)), TScalaInt,
    BaseApplyInfix(Var("n").typed(TScalaInt), "+", BaseLit(q"1", TScalaInt), TScalaInt)
  )
  val incMain: FunctionDef = FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Call(Name("inc"), Seq(BaseLit(q"0", TScalaInt))).resolved(incFun)
  ).addAnnotation(MainFunctionAnno)
  val incModule: Module = module(incFun, incMain)


  val factRecCall: Call = Call(Name("fact"), Seq(BaseApplyInfix(Var("n").typed(TScalaInt), "-", BaseLit(q"1", TScalaInt), TScalaInt))).typed(TScalaInt)
  val factFun: FunctionDef = FunctionDef(None, Name("fact"), Seq(Param(Name("n"), TScalaInt)), TScalaInt,
    If(BaseApplyInfix(Var("n").typed(TScalaInt), "==", BaseLit(q"1", TScalaInt), TScalaBoolean),
      BaseLit(q"1", TScalaInt),
      BaseApplyInfix(Var("n").typed(TScalaInt), "*", factRecCall, TScalaInt)
    )
  )
  factRecCall.resolved(factFun)
  val factMain: FunctionDef = FunctionDef(None, Name("main"), Seq(), TScalaInt,
    Call(Name("fact"), Seq(BaseLit(q"3", TScalaInt))).resolved(factFun)
  ).addAnnotation(MainFunctionAnno)
  val factModule: Module = module(factFun, factMain)


  val recPlusCall: Call = Call(Name("plus"), Seq(Var(Name("pred")), Var(Name("n"))))
  val plusFun: FunctionDef = FunctionDef(None, Name("plus"), Seq(Param(Name("m"), TNat), Param(Name("n"), TNat)), TNat,
    Match(Var(Name("m")), Seq(
      ConstructorPattern(Name("Zero"), Seq()).resolved(Zero) ->
        Var("n"),
      ConstructorPattern(Name("Succ"), Seq(Name("pred"))).resolved(Succ) ->
        CallSucc(recPlusCall)
    ))
  )
  recPlusCall.resolved(plusFun)
  val plusMain: FunctionDef = FunctionDef(None, Name("main"), Seq(), TNat,
    Call(Name("plus"), Seq(
      CallSucc(CallSucc(CallSucc(CallZero))),
      CallSucc(CallSucc(CallZero))
    )).resolved(plusFun)
  ).addAnnotation(MainFunctionAnno)
  val plusModule: Module = module(Nat, plusFun, plusMain)

  val plusRealMain: FunctionDef = FunctionDef(None, Name("main"), Seq(Param(Name("m"), TNat), Param(Name("n"), TNat)), TNat,
    Call(Name("plus"), Seq(Var(Name("m")), Var(Name("n")))).resolved(plusFun)
  ).addAnnotation(MainFunctionAnno)
  val plusRealModule: Module = module(Nat, plusFun, plusRealMain)
}
