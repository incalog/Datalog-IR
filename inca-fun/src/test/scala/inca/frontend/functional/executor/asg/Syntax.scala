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


def makeProg(from: Int, to: Int, step: Int): List[Def] = {
  if (from < to) {
    val next = from + step
    val lineDefs: List[Def] = makeLine(from, next)
    val circleDef = DefV(s"a$from", Add(Var(s"a$next"), Num(from)))
    val recDefs: List[Def] = makeProg(next, to, step)
    lineDefs ::: (circleDef :: recDefs)
  } else {
    val circleDef = DefV(s"a$from", Add(Var("a0"), Num(from)))
    List(circleDef)
  }
}

def makeLine(i: Int, to: Int): List[Def] = {
  if (i < to) {
    val n = i + 1
    val d = DefV(s"a$i", Add(Var(s"a$n"), Num(i)))
    d :: makeLine(n, to)
  } else {
    List()
  }
}

/**
 * s = step
 * m = size
 *
 * ________________  ____________ _____...
 * |                 |            |
 * |                 v            v
 * a_0 -> a_1 -> ... a_s -> ... ->  ... -> a_{m+1}
 * ^                                        |
 * |________________________________________|
 */
def generateProgram(size: Int, step: Int): ADT =
  val nodes = makeProg(0, size, step)
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

