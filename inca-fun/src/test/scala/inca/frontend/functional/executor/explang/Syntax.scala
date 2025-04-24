package inca.frontend.functional.executor.explang

import inca.ir.execution.{ADT, ADTConvertible}

enum Exp extends ADTConvertible:
    case BoolLit(value: Int) // Use Ints, because Booleans lower to ints
    case And(lhs: Exp, rhs: Exp)
    case Or(lhs: Exp, rhs: Exp)
    case Not(expr: Exp)
    case If(cond: Exp, thenBranch: Exp, elseBranch: Exp)
    case NumLit(value: Int)
    case Plus(lhs: Exp, rhs: Exp)
    case Mult(lhs: Exp, rhs: Exp)
    case BiImplication(lhs: Exp, rhs: Exp)
    case Implication(lhs: Exp, rhs: Exp)

    def toADT: ADT = this match
        case BoolLit(value)     => ADT("Exp", "BoolLit", Seq(value))
        case Not(expr)          => ADT("Exp", "Not", Seq(expr.toADT))
        case And(lhs, rhs)      => ADT("Exp", "And", Seq(lhs.toADT, rhs.toADT))
        case Or(lhs, rhs)       => ADT("Exp", "Or", Seq(lhs.toADT, rhs.toADT))
        case If(c, t, e)        => ADT("Exp", "If", Seq(c.toADT, t.toADT, e.toADT))
        case NumLit(value)      => ADT("Exp", "NumLit", Seq(value))
        case Plus(lhs, rhs)     => ADT("Exp", "Plus", Seq(lhs.toADT, rhs.toADT))
        case Mult(lhs, rhs)     => ADT("Exp", "Mult", Seq(lhs.toADT, rhs.toADT))
        case Implication(l, r)  => ADT("Exp", "Implication", Seq(l.toADT, r.toADT))
        case BiImplication(l, r)=> ADT("Exp", "BiImplication", Seq(l.toADT, r.toADT))

object Exp:
    def True: BoolLit = BoolLit(1)
    def False: BoolLit = BoolLit(0)
    def Num(value: Int): NumLit = NumLit(value)

val prog1: ADT =
    Exp.If(
        Exp.Implication(
            Exp.And(Exp.True, Exp.False),
            Exp.True
        ),
        Exp.Plus(Exp.Num(1), Exp.Num(2)),
        Exp.Mult(Exp.Num(3), Exp.Num(4))
    ).toADT