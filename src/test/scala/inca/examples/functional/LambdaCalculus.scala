package inca.examples.functional

import inca.examples.functional.Code.module

object LambdaCalculus {
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

  val Val2_code =
    s"""data Val = VNum(Int) | VFun(String, Exp)
      |""".stripMargin

  val typeOfFunction =
    s"""def typeOf(ctx: Ctx, exp: TExp): MaybeType = exp match {
      |  case TNum(v) => SomeType(TInt())
      |  case TLam(n, ty, b) =>
      |    typeOf(BindCtx(n, ty, ctx), b) match {
      |      case SomeType(ty2) => SomeType(TFun(ty, ty2))
      |      case NoType() => NoType()
      |    }
      |  case TApp(fun, arg) =>
      |    typeOf(ctx, fun) match {
      |      case SomeType(funty) =>
      |        funty match {
      |          case TInt() => NoType()
      |          case TFun(ty1, ty2) =>
      |            typeOf(ctx, arg) match {
      |              case SomeType(argty) =>
      |                if (eqType(argty, ty1)) SomeType(ty2)
      |                else NoType()
      |              case NoType() => NoType()
      |            }
      |      }
      |      case NoType() => NoType()
      |    }
      |  case TVar(n) => ctxLookup(ctx, n)
      |}
      |""".stripMargin

  val typeOfRelation =
    s"""def typeOf(ctx: Ctx, exp: TExp): Option[Type] = exp match {
      |  case TNum(v) => Some(TInt())
      |  case TLam(n, ty, b) =>
      |    typeOf(BindCtx(n, ty, ctx), b) match {
      |      case None => None
      |      case Some(ty2) => Some(TFun(ty, ty2))
      |    }
      |  case TApp(fun, arg) => typeOf(ctx, fun) match {
      |    case None => None
      |    case Some(funty) => funty match {
      |      case TInt() => None
      |      case TFun(ty1, ty2) =>
      |        typeOf(ctx, arg) match {
      |          case None => None
      |          case Some(argty) =>
      |            if (eqType(argty, ty1))
      |              Some(ty2)
      |            else
      |              None
      |        }
      |    }
      |  }
      |  case TVar(n) => ctxLookup(ctx, n)
      |}
      |""".stripMargin

  val typeOfRelationUndefs =
    s"""def typeOf(ctx: Ctx, exp: TExp): Option[Type] = exp match {
      |  case TNum(v) => Some(TInt())
      |  case TLam(n, ty, b) =>
      |    let extCtx = BindCtx(n, ty, ctx) in
      |      let mt = typeOf(extCtx, b) in mt match {
      |        case None => Some(TInt())
      |        case Some(ty2) => Some(TFun(ty, ty2))
      |      }
      |  case TApp(fun, arg) => typeOf(ctx, fun) match {
      |    case None => Some(TInt())
      |    case Some(funty) => funty match {
      |      case TInt() => Some(TInt())
      |      case TFun(ty1, ty2) =>
      |        typeOf(ctx, arg) match {
      |          case None => Some(TInt())
      |          case Some(argty) =>
      |            if (eqType(argty, ty1))
      |              Some(ty2)
      |            else
      |              Some(TInt())
      |        }
      |    }
      |  }
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
    s"""def ctxLookup(ctx: Ctx, n: String): MaybeType = ctx match {
      |  case EmptyCtx() => NoType()
      |  case BindCtx(n1, ty, rest) =>
      |    if (n1 == n) SomeType(ty)
      |    else ctxLookup(rest, n)
      |}
      |""".stripMargin

  val ctxLookupRelation =
    s"""def ctxLookup(ctx: Ctx, n: String): Option[Type] = ctx match {
      |  case EmptyCtx() => None
      |  case BindCtx(n1, ty, rest) =>
      |    if (n1 == n) Some(ty)
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

  val typeOfRelModule: String = module(
    TExp_code,
    Type_code,
    Ctx_code,
    s"""@main def main(exp: TExp): Option[Type] = typeOf(EmptyCtx(), exp)
      |""".stripMargin,
    typeOfRelation,
    eqTypeFunction,
    ctxLookupRelation
  )

  val typeOfRelModuleUndefs: String = module(
    TExp_code,
    Type_code,
    Ctx_code,
    s"""@main def main(exp: TExp): Option[Type] = typeOf(EmptyCtx(), exp)
      |""".stripMargin,
    typeOfRelationUndefs,
    eqTypeFunction,
    ctxLookupRelation
  )

  val eraseFunciton =
    s"""def erase(texp: TExp): Exp = texp match {
      |  case TNum(v) => Num(v)
      |  case TLam(n, ty, b) => Lam(n, erase(b))
      |  case TApp(fun, arg) => App(erase(fun), erase(arg))
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
      |  case App(fun, arg) => interp(env, fun) match {
      |    case SomeVal(funv) => funv match {
      |      case VClosure(param, body, fenv) => interp(env, arg) match {
      |        case SomeVal(argv) => interp(BindEnv(param, argv, fenv), body)
      |        case NoVal() => NoVal()
      |      }
      |      case VNum(v) => NoVal()
      |    }
      |    case NoVal() => NoVal()
      |  }
      |  case Var(n) => envLookup(env, n)
      |}
      |
      |""".stripMargin

  val envLookupFunction =
    s"""def envLookup(env: Env, n: String): MaybeVal = env match {
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
      |    interp(EmptyEnv(), erase(texp))
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
