package inca.ir.extension.datamatch

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.data.Deconstruct
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, NegExtensionalCall, Term}

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:

  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(data.IR, disjunction.IR)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case Match(matchee, cases) =>
      val alternatives = cases.map { case Case(name, patVars, body) =>
        DisjunctionAlternative(Deconstruct(matchee, name, patVars) +: body.flatMap(visitAtom))
      }
      Seq(Disjunction((alternatives)))
    case _ => super.visitAtom(atom)
  }

// r(T) :- guard(x), x match { case Zero() => a1 a2 a3; case Succ(p) => a4 a5 a6 }

// r(T) :- guard(x), true == x.isInstanceOf[Zero], a1 a2 a3
// r(T) :- guard(x), true == x.isInstanceOf[Succ], p = x.asInstanceOf[Succ].param a4 a5 a6