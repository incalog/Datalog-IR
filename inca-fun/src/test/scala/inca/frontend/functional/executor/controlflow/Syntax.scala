package inca.frontend.functional.executor.controlflow

import inca.ir.execution.ADT

enum Exp:
  case Var(name: String) extends Exp
  case Num(value: Int) extends Exp
  case GreaterThan(left: Exp, right: Exp) extends Exp
  case Mul(left: Exp, right: Exp) extends Exp
  case Add(left: Exp, right: Exp) extends Exp
  case Sub(left: Exp, right: Exp) extends Exp

  def toADT: ADT = this match {
    case Var(name) => ADT("Exp", "Var", Seq(name))
    case Num(value) => ADT("Exp", "Num", Seq(value))
    case GreaterThan(left, right) => ADT("Exp", "GreaterThan", Seq(left.toADT, right.toADT))
    case Mul(left, right) => ADT("Exp", "Mul", Seq(left.toADT, right.toADT))
    case Add(left, right) => ADT("Exp", "Add", Seq(left.toADT, right.toADT))
    case Sub(left, right) => ADT("Exp", "Sub", Seq(left.toADT, right.toADT))
  }

enum Stm:
  case Assign(variable: String, exp: Exp)
  case Skip()
  case Sequence(first: Stm, second: Stm)
  case If(condition: Exp, thenStm: Stm, elseStm: Stm)
  case While(condition: Exp, body: Stm)

  def toADT: ADT = this match {
    case Assign(variable, exp) => ADT("Stm", "Assign", Seq(variable, exp.toADT))
    case Skip() => ADT("Stm", "Skip", Seq())
    case Sequence(first, second) => ADT("Stm", "Sequence", Seq(first.toADT, second.toADT))
    case If(condition, thenStm, elseStm) => ADT("Stm", "If", Seq(condition.toADT, thenStm.toADT, elseStm.toADT))
    case While(condition, body) => ADT("Stm", "While", Seq(condition.toADT, body.toADT))
  }

import Stm._
import Exp._


def nestedWhileProgramAST(nestings: Int, repetitions: Int): Stm =
  def nestedWhile(levels: Int): Stm =
    if (levels == 0)
      Sequence(
        Assign("x", Add(Var("x"), Num(-1))),
        Assign("x", Add(Var("x"), Num(1)))
      )
    else
      While(
        GreaterThan(Var("x"), Num(0)),
        nestedWhile(levels - 1)
      )

  def sequence(s: () => Stm, counts: Int): Stm =
    if (counts == 0)
      s()
    else
      Sequence(s(), sequence(s, counts - 1))

  Sequence(Assign("x", Num(1)), sequence(() => nestedWhile(nestings), repetitions))


def nestedWhileProgram(nestings: Int, repetitions: Int): ADT =
  nestedWhileProgramAST(nestings, repetitions).toADT


val prog1: ADT =
  Sequence(
    Assign("x", Num(2)),
    Sequence(
      Assign("y", Num(2)),
      While(GreaterThan(Var("x"), Num(1)),
        Sequence(
          Assign("y", Add(Var("x"), Var("y"))),
          Sequence(
            Assign("z", Num(12)),
            Assign("x", Add(Var("x"), Num(2)))))))).toADT


