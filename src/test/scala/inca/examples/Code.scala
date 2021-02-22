package inca.examples

import inca.examples.ADT.{Ctx_code, Env_code, Exp_code, MaybeType_code, MaybeVal_code, Nat_code, TExp_code, Type_code, Val_code}

object Code {
  def module(content: String*): String =
    s"""module Main
       |${content.mkString("\n")}
       |""".stripMargin

  val baseExample: String = module(
    s"""@main def main(): `Int` = `7 + (12 * 3)`
       |""".stripMargin
  )

  val baseExample2a: String = module(
    s"""@main def main(): `Int` = `7` + (`12` * `3`)
       |""".stripMargin
  )

  val baseExample2b: String = module(
    s"""@main def main(): `Int` = 7 + (12 * 3)
       |""".stripMargin
  )

  val varExample: String = module(
    s"""@main def main(): `Int` =
       |  let x = 7 in
       |    let y = 3 in
       |      x + (12 * y)
       |""".stripMargin
  )

  val ifExample: String = module(
    s"""@main def main(): `Int` =
       |  let x = 7 in
       |    if (x > 0)
       |      x
       |    else
       |      x * -1
       |""".stripMargin
  )

  val ifExample2: String = module(
    s"""@main def main(): `Int` =
       |  let x = 7 in
       |    let y = -3 in
       |      (if (x > 0) x else x * -1) + (if (y > 0) y else y * -1)
       |""".stripMargin
  )

  val incModule: String = module(
    s"""def inc(n: `Int`): `Int` = n + 1""",
    s"""@main def main(): `Int` = inc(0)"""
  )

  val factModule: String = module(
    s"""def fact(n: `Int`): `Int` =
       |  if (n == 1)
       |    1
       |  else
       |    n * fact(n - 1)
       |""".stripMargin,
    s"""@main def main(n: `Int`): `Int` =
       |  fact(n)
       |""".stripMargin
  )

  val plusModule: String = module(
    Nat_code,
    s"""def plus(m: Nat, n: Nat): Nat = m match {
       |  case Zero() => n
       |  case Succ(pred) => Succ(plus(pred, n))
       |}
       |""".stripMargin,
    s"""@main def main(): Nat =
       |  plus(Succ(Succ(Succ(Zero()))), Succ(Succ(Zero())))
       |""".stripMargin
  )

  val plusNoMainModule: String = module(
    Nat_code,
    s"""@main def plus(m: Nat, n: Nat): Nat = m match {
       |  case Zero() => n
       |  case Succ(pred) => Succ(plus(pred, n))
       |}
       |""".stripMargin,
  )

  val plusRealModule: String = module(
    Nat_code,
    s"""def plus(m: Nat, n: Nat): Nat = m match {
       |  case Zero() => n
       |  case Succ(pred) => Succ(plus(pred, n))
       |}
       |""".stripMargin,
    s"""@main def main(m: Nat, n: Nat): Nat =
       |  plus(m, n)
       |""".stripMargin
  )

  val fibModule: String =
    s"""module Fib
       |@main def main(n: `Int`): `Int` =
       |  fib(n)
       |def fib(n: `Int`): `Int` =
       |  if (n == 0)
       |    0
       |  else if (n == 1)
       |    1
       |  else
       |    fib(n - 1) + fib(n - 2)
       |""".stripMargin

  val typeOfFunction =
    s"""def typeOf(ctx: Ctx, exp: TExp): MaybeType = exp match {
       |  case TNum(v) => SomeType(TInt())
       |  case TLam(n, ty, b) =>
       |    let extCtx = BindCtx(n, ty, ctx) in
       |      let mbty2 = typeOf(extCtx, b) in
       |        mbty2 match {
       |          case SomeType(ty2) => SomeType(TFun(ty, ty2))
       |          case NoType() => NoType()
       |        }
       |  case TApp(fun, arg) =>
       |    let mbfunty = typeOf(ctx, fun) in
       |      mbfunty match {
       |        case SomeType(funty) =>
       |          funty match {
       |            case TInt() => NoType()
       |            case TFun(ty1, ty2) =>
       |              let mbargty = typeOf(ctx, arg) in
       |                mbargty match {
       |                  case SomeType(argty) =>
       |                    if (eqType(argty, ty1)) SomeType(ty2)
       |                    else NoType()
       |                  case NoType() => NoType()
       |                }
       |          }
       |        case NoType() => NoType()
       |      }
       |  case TVar(n) => ctxLookup(ctx, n)
       |}
       |""".stripMargin

  val eqTypeFunction =
    s"""def eqType(ty1: Type, ty2: Type): `Boolean` = ty1 match {
       |  case TInt() => ty2 match {
       |    case TInt() => true
       |    case TFun(ofty1, ofty2) => false
       |  }
       |  case TFun(fty1, fty2) => ty2 match {
       |    case TInt() => false
       |    case TFun(ofty1, ofty2) =>
       |     eqType(fty1, ofty1) && eqType(fty2, ofty2)
       |  }
       |}
       |""".stripMargin

  val ctxLookupFunction =
    s"""def ctxLookup(ctx: Ctx, n: `String`): MaybeType = ctx match {
       |  case EmptyCtx() => NoType()
       |  case BindCtx(n1, ty, rest) =>
       |    if (n1 == n) SomeType(ty)
       |    else ctxLookup(rest, n)
       |}
       |""".stripMargin

  val typeOfModule: String = module(
    TExp_code,
    Type_code,
    MaybeType_code,
    Ctx_code,
    s"""@main def main(exp: TExp): MaybeType = typeOf(EmptyCtx(), exp)
      |""".stripMargin,
    typeOfFunction,
    eqTypeFunction,
    ctxLookupFunction
  )

  val eraseFunciton =
    s"""def erase(texp: TExp): Exp = texp match {
       |  case TNum(v) => Num(v)
       |  case TLam(n, ty, b) =>
       |    let eb = erase(b) in
       |      Lam(n, eb)
       |  case TApp(fun, arg) =>
       |    let efun = erase(fun) in
       |      let earg = erase(arg) in
       |        App(efun, earg)
       |  case TVar(n) => Var(n)
       |}
       |""".stripMargin

  val eraseModule: String = module(
    TExp_code,
    Type_code,
    Exp_code,
    s"""@main def main(exp: TExp): Exp = erase(exp)
       |""".stripMargin,
    eraseFunciton
  )

  val interpFunction =
    s"""def interp(env: Env, exp: Exp): MaybeVal = exp match {
       |  case Num(v) => SomeVal(VNum(v))
       |  case Lam(n, b) => SomeVal(VClosure(n, b, env))
       |  case App(fun, arg) =>
       |    let mbfunv = interp(env, fun) in
       |      mbfunv match {
       |        case SomeVal(funv) =>
       |          funv match {
       |            case VClosure(param, body, fenv) =>
       |              let mbargv = interp(env, arg) in
       |                mbargv match {
       |                  case SomeVal(argv) =>
       |                    let extEnv = BindEnv(param, argv, fenv) in
       |                      interp(extEnv, body)
       |                  case NoVal() => NoVal()
       |                }
       |            case VNum(v) => NoVal()
       |          }
       |        case NoVal() => NoVal()
       |      }
       |  case Var(n) => envLookup(env, n)
       |}
       |
       |""".stripMargin

  val envLookupFunction =
    s"""def envLookup(env: Env, n: `String`): MaybeVal = env match {
       |  case EmptyEnv() => NoVal()
       |  case BindEnv(n1, v, rest) =>
       |    if (n1 == n) SomeVal(v)
       |    else envLookup(rest, n)
       |}
       |""".stripMargin

  val interpModule: String = module(
    Exp_code,
    Env_code,
    Val_code,
    MaybeVal_code,
    s"""@main def main(exp: Exp): MaybeVal = interp(EmptyEnv(), exp)
       |""".stripMargin,
    interpFunction,
    envLookupFunction
  )

  val completeLCModule: String = module(
    TExp_code,
    Type_code,
    Ctx_code,
    MaybeType_code,
    Exp_code,
    Val_code,
    MaybeVal_code,
    Env_code,
    s"""@main def main(texp: TExp): MaybeVal = typeOf(EmptyCtx(), texp) match {
       |  case SomeType(ty) =>
       |    let exp = erase(texp) in
       |      interp(EmptyEnv(), exp)
       |  case NoType() => NoVal()
       |}
       |""".stripMargin,
    typeOfFunction,
    ctxLookupFunction,
    eqTypeFunction,
    eraseFunciton,
    interpFunction,
    envLookupFunction
  )
}
