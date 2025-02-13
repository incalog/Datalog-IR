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

/*private def generateAST(i: Int, end: Int): Exp =
  if (i < end)
    Cons(
      DefV(s"a$i", Var(s"a${i+1}")),
      generateAST(i+1, end)
    )
  else
    Num(3)

def generateProg(size: Int) = generateAST(0, size).toADT
*/

def generateProgram(size: Int, step: Int): ADT =
  val nodes = Range.inclusive(1, size).flatMap { i =>
    val forward = DefV(s"a${i - 1}", Var(s"a$i"))
    if (i % step == 0)
      val back = DefV(s"a$i", Var(s"a${i - step}"))
      Seq(forward, back)
    else
      Seq(forward)
  }
  nodes.foldRight[DefList](Nil()) {
    case (node, acc) => Cons(node, acc)
  }.toADT

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

