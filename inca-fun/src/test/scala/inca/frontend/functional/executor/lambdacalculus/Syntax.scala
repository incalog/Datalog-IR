package inca.frontend.functional.executor.lambdacalculus

import inca.ir.execution.ADT

enum Type:
  case TInt()
  case TFun(argType: Type, returnType: Type)

  def toADT: ADT = this match
    case TInt() => ADT("Type", "TInt", Seq())
    case TFun(argType, returnType) =>
      ADT("Type", "TFun", Seq(argType.toADT, returnType.toADT))

enum TExp:
  case TNum(value: Int)
  case TLam(param: String, paramType: Type, body: TExp)
  case TApp(func: TExp, arg: TExp)
  case TVar(name: String)

  def toADT: ADT = this match
    case TNum(value) => ADT("TExp", "TNum", Seq(value))
    case TLam(param, paramType, body) => ADT("TExp", "TLam", Seq(param, paramType.toADT, body.toADT))
    case TApp(func, arg) => ADT("TExp", "TApp", Seq(func.toADT, arg.toADT))
    case TVar(name) => ADT("TExp", "TVar", Seq(name))


enum Exp:
  case Num(value: Int)
  case Lam(param: String, body: Exp)
  case App(func: Exp, arg: Exp)
  case Var(name: String)

  def toADT: ADT = this match
    case Num(value) => ADT("Exp", "Num", Seq(value))
    case Lam(param, body) => ADT("Exp", "Lam", Seq(param, body.toADT))
    case App(func, arg) => ADT("Exp", "App", Seq(func.toADT, arg.toADT))
    case Var(name) => ADT("Exp", "Var", Seq(name))

import Exp._
import TExp._
import Type._

val interpProg1: ADT = App(Lam("y", Lam("x", Var("y"))), Num(1)).toADT
val typeProg1: ADT = TApp(TLam("x", TInt(), TVar("x")), TNum(1337)).toADT

private def generateAST(i: Int, end: Int): TExp =
  if (i < end)
    TApp(TLam("x" + i, TInt(), TVar("x" + i)), generateAST(i+1, end))
  else
    TNum(1337)

def generateTypedProg(size: Int) = generateAST(0, size).toADT
