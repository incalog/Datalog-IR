package inca.frontend.functional.executor.typechecker

import inca.ir.execution.{ADT, ADTConvertible}

// The syntax of the language to analyse
enum BindingList extends ADTConvertible:
  case Nil
  case Cons(name: String, exp: Exp, next: BindingList)

  def toADT: ADT = this match
    case Nil => ADT("BindingList", "Nil", Seq())
    case Cons(name, exp, next) => ADT("BindingList", "Cons", Seq(name, exp.toADT, next.toADT))

enum Exp extends ADTConvertible:
  case Num(value: Int)
  case Var(name: String)
  case Add(left: Exp, right: Exp)
  case Lam(param: String, paramType: Type, body: Exp)
  case App(func: Exp, arg: Exp)
  case Let(name: String, value: Exp, body: Exp)
  case LetStar(bindings: BindingList, body: Exp)

  def toADT: ADT = this match
    case Num(value) => ADT("Exp", "Num", Seq(value))
    case Var(name) => ADT("Exp", "Var", Seq(name))
    case Add(left, right) => ADT("Exp", "Add", Seq(left.toADT, right.toADT))
    case Lam(param, paramType, body) => ADT("Exp", "Lam", Seq(param, paramType.toADT, body.toADT))
    case App(func, arg) => ADT("Exp", "App", Seq(func.toADT, arg.toADT))
    case Let(name, value, body) => ADT("Exp", "Let", Seq(name, value.toADT, body.toADT))
    case LetStar(bindings, body) => ADT("Exp", "LetStar", Seq(bindings.toADT, body.toADT))

enum Type extends ADTConvertible:
  case TInt
  case TFun(from: Type, to: Type)

  def toADT: ADT = this match
    case TInt => ADT("Type", "TInt", Seq())
    case TFun(from, to) => ADT("Type", "TFun", Seq(from.toADT, to.toADT))

import Exp._
import Type._

val prog0: ADT = Lam("x", TInt, Add(Num(1), Var("x"))).toADT

val prog1: ADT = Lam("m", TFun(TFun(TInt, TInt), TFun(TInt, TInt)),
  Lam("n", TFun(TFun(TInt, TInt), TFun(TInt, TInt)),
    Lam("f", TFun(TInt, TInt),
      Lam("x", TInt,
        App(
          App(Var("m"), Var("f")),
          App(App(Var("n"), Var("f")), Var("x"))
        ))))).toADT

private def generateAST(i: Int, end: Int): Exp =
  if (i < end)
    Let("x" + i, Num(i), generateAST(i+1, end))
  else
    Add(Var("x0"), Var("x" + (end-1)))

def generateProgram(size: Int): ADT = generateAST(0, size).toADT