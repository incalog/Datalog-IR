package inca.ir.extension.datamatch

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.data.Deconstruct
import inca.ir.extension.disjunction.Disjunction
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, NegExtensionalCall, Term}

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:

  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(data.IR)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case Match(matchee, cases) =>
      val alternatives = cases.map { case Case(name, patVars, body) =>
        Deconstruct(matchee, name, patVars) +: body
      }
      Seq(Disjunction((alternatives)))
    case _ => super.visitAtom(atom)
  }
