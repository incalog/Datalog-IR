package inca.hazel.edb

import truediff.Diffable
import truediff.macros.diffable

@diffable sealed trait Exp extends Diffable
@diffable case class EHole() extends Exp
@diffable case class EVar(x: String) extends Exp
@diffable case class ELam(param: String, ty: TypeAnno, body: Exp) extends Exp
@diffable case class EAp(lhs: Exp, rhs: Exp) extends Exp
@diffable case class ELet(name: String, defn: Exp, body: Exp) extends Exp
@diffable case class ENum(num: Int) extends Exp
@diffable case class EPlus(lhs: Exp, rhs: Exp) extends Exp
@diffable case class ETrue() extends Exp
@diffable case class EFalse() extends Exp
@diffable case class EIf(guard: Exp, lhs: Exp, rhs: Exp) extends Exp
@diffable case class EPair(lhs: Exp, rhs: Exp) extends Exp
@diffable case class EProjL(exp: Exp) extends Exp
@diffable case class EProjR(exp: Exp) extends Exp
