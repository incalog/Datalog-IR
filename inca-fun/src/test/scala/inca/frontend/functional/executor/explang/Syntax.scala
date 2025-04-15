package inca.frontend.functional.executor.explang

import inca.ir.execution.{ADT, ADTConvertible}

enum Exp extends ADTConvertible:
    case True
    case False
    case And(lhs: Exp, rhs: Exp)
    case Or(lhs: Exp, rhs: Exp)
    case Not(expr: Exp)
    case If(cond: Exp, thenBranch: Exp, elseBranch: Exp)
    case Num(value: Int)
    case Plus(lhs: Exp, rhs: Exp)
    case Mult(lhs: Exp, rhs: Exp)
    case BiImplication(lhs: Exp, rhs: Exp)
    case Implication(lhs: Exp, rhs: Exp)

    def toADT: ADT = this match {
        case True               => ADT("Exp", "True", Seq())
        case False              => ADT("Exp", "False", Seq())
        case Not(expr)          => ADT("Exp", "Not", Seq(expr.toADT))
        case And(lhs, rhs)      => ADT("Exp", "And", Seq(lhs.toADT, rhs.toADT))
        case Or(lhs, rhs)       => ADT("Exp", "Or", Seq(lhs.toADT, rhs.toADT))
        case If(c, t, e)        => ADT("Exp", "If", Seq(c.toADT, t.toADT, e.toADT))
        case Num(value)         => ADT("Exp", "Num", Seq(value))
        case Plus(lhs, rhs)     => ADT("Exp", "Plus", Seq(lhs.toADT, rhs.toADT))
        case Mult(lhs, rhs)     => ADT("Exp", "Mult", Seq(lhs.toADT, rhs.toADT))
        case Implication(l, r)  => ADT("Exp", "Implication", Seq(l.toADT, r.toADT))
        case BiImplication(l, r)=> ADT("Exp", "BiImplication", Seq(l.toADT, r.toADT))
    }

val prog1: ADT =
    Exp.If(
        Exp.Implication(
            Exp.And(Exp.True, Exp.False),
            Exp.True
        ),
        Exp.Plus(Exp.Num(1), Exp.Num(2)),
        Exp.Mult(Exp.Num(3), Exp.Num(4))
    ).toADT