package inca.examples

import inca.examples.ADT._
import inca.frontend.functional.core._

import scala.meta.XtensionQuasiquoteTerm

object AST {
  def module(content: ModuleContent*): Module =
    Module(Name("Main"), Seq(), content)

  val baseExample: Module = module(FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(), TScalaInt,
    BaseLit(q"7 + (12 * 3)", TScalaInt)
  ))
  val baseExample2: Module = module(FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(), TScalaInt,
    BaseApplyInfix(BaseLit(q"7", TScalaInt), "+",
      BaseApplyInfix(BaseLit(q"12", TScalaInt), "*", BaseLit(q"3", TScalaInt))
    )
  ))

  val varExample: Module = module(FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, BaseLit(q"7", TScalaInt),
      Let(Seq(Name("y")), None, BaseLit(q"3", TScalaInt),
        BaseApplyInfix(Var("x"), "+",
          BaseApplyInfix(BaseLit(q"12", TScalaInt), "*", Var("y"))
        )
      )
    )
  ))

  val ifExample: Module = module(FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, BaseLit(q"7", TScalaInt),
      If(BaseApplyInfix(Var("x"), ">", BaseLit(q"0", TScalaInt)),
        Var("x"),
        BaseApplyInfix(Var("x"), "*", BaseLit(q"-1", TScalaInt))
      )
    )
  ))

  val ifExample2: Module = module(FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(), TScalaInt,
    Let(Seq(Name("x")), None, BaseLit(q"7", TScalaInt),
      Let(Seq(Name("y")), None, BaseLit(q"-3", TScalaInt),
        BaseApplyInfix(
          If(BaseApplyInfix(Var("x"), ">", BaseLit(q"0", TScalaInt)),
            Var("x"),
            BaseApplyInfix(Var("x"), "*", BaseLit(q"-1", TScalaInt))
          ),
          "+",
          If(BaseApplyInfix(Var("y"), ">", BaseLit(q"0", TScalaInt)),
            Var("y"),
            BaseApplyInfix(Var("y"), "*", BaseLit(q"-1", TScalaInt))
          )
        )
      )
    )
  ))


  val incFun: FunctionDef = FunctionDef(Seq(), None, Name("inc"), Seq(Param(Name("n"), TScalaInt)), TScalaInt,
    BaseApplyInfix(Var("n"), "+", BaseLit(q"1", TScalaInt))
  )
  val incMain: FunctionDef = FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(), TScalaInt,
    Call(Var(Name("inc")), Seq(BaseLit(q"0", TScalaInt)))
  )
  val incModule: Module = module(incFun, incMain)


  val factFun: FunctionDef = FunctionDef(Seq(), None, Name("fact"), Seq(Param(Name("n"), TScalaInt)), TScalaInt,
    If(BaseApplyInfix(Var("n"), "==", BaseLit(q"1", TScalaInt)),
      BaseLit(q"1", TScalaInt),
      BaseApplyInfix(Var("n"), "*",
        Call(Var(Name("fact")), Seq(BaseApplyInfix(Var("n"), "-", BaseLit(q"1", TScalaInt)))))
    )
  )
  val factMain: FunctionDef = FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(Param(Name("n"), TScalaInt)), TScalaInt,
    Call(Var(Name("fact")), Seq(Var("n")))
  )
  val factModule: Module = module(factFun, factMain)


  val plusFun: FunctionDef = FunctionDef(Seq(), None, Name("plus"), Seq(Param(Name("m"), TNat), Param(Name("n"), TNat)), TNat,
    Match(Var(Name("m")), Seq(
      ConstructorPattern(Name("Zero"), Seq()) ->
        Var("n"),
      ConstructorPattern(Name("Succ"), Seq(Name("pred"))) ->
        Call(Var(Name("Succ")), Seq(Call(Var(Name("plus")), Seq(Var(Name("pred")), Var(Name("n"))))))
    ))
  )
  val plusMain: FunctionDef = FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(), TNat,
    Call(Var(Name("plus")), Seq(
      Call(Var(Name("Succ")), Seq(Call(Var(Name("Succ")), Seq(Call(Var(Name("Succ")), Seq(Call(Var(Name("Zero")), Seq()))))))),
      Call(Var(Name("Succ")), Seq(Call(Var(Name("Succ")), Seq(Call(Var(Name("Zero")), Seq())))))
    ))
  )
  val plusModule: Module = module(Nat, plusFun, plusMain)

  val plusRealMain: FunctionDef = FunctionDef(Seq(MainFunctionAnno), None, Name("main"), Seq(Param(Name("m"), TNat), Param(Name("n"), TNat)), TNat,
    Call(Var(Name("plus")), Seq(Var(Name("m")), Var(Name("n"))))
  )
  val plusRealModule: Module = module(Nat, plusFun, plusRealMain)
}
