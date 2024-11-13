package inca.casestudy.interval.edb

import truediff.Diffable
import truediff.macros.diffable

@diffable sealed trait Exp extends Diffable

@diffable case class Var(name: String) extends Exp

@diffable case class Num(value: Int) extends Exp

@diffable case class Add(lhs: Exp, rhs: Exp) extends Exp

@diffable case class GT(lhs: Exp, rhs: Exp) extends Exp

@diffable sealed trait Stmt extends Diffable

@diffable case class Assign(name: String, exp: Exp) extends Stmt

@diffable case class Skip() extends Stmt

@diffable case class Sequence(s1: Stmt, s2: Stmt) extends Stmt

@diffable case class While(cond: Exp, body: Stmt) extends Stmt

@diffable case class Exit() extends Stmt
