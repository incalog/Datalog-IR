package inca.frontend.functional.executor.asg

import inca.ir.execution.ADT

enum Exp:
  case Num(value: Int) extends Exp
  case Var(name: String) extends Exp
  case Add(left: Exp, right: Exp) extends Exp

  def toADT: ADT = this match {
    case Num(value) => ADT("Exp", "Num", Seq(value))
    case Var(name) => ADT("Exp", "Var", Seq(name))
    case Add(left, right) => ADT("Exp", "Add", Seq(left.toADT, right.toADT))
  }

enum Def:
  case DefV(name: String, exp: Exp)

  def toADT: ADT = this match {
    case DefV(name, exp) => ADT("Def", "DefV", Seq(name, exp.toADT))
  }

enum DefList:
  case Nil() extends DefList
  case Cons(head: Def, tail: DefList) extends DefList

  def toADT: ADT = this match {
    case Nil() => ADT("DefList", "Nil", Seq())
    case Cons(head, tail) => ADT("DefList", "Cons", Seq(head.toADT, tail.toADT))
  }

import DefList._
import Exp._
import Def._

val prog1: ADT =
  Cons(
    DefV("x", Var("y")),
    Cons(
      DefV("y", Add(Var("x"), Num(3))),
      Cons(
        DefV("z", Add(Var("x"), Num(4))),
        Nil()
      )
    )
  ).toADT

