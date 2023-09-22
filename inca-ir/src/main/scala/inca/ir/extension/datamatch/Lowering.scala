package inca.ir.extension.datamatch

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.data.Deconstruct
import inca.ir.extension.disjunction.Disjunction
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, NegExtensionalCall, Term}

import scala.collection.mutable.ListBuffer

object Lowering:
  def apply[S <: IR, T <: BaseIR](srcIR: S, trgIR: T): Lowering[S, T] = new Lowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

trait Lowering[S <: IR, T <: BaseIR] extends BaseLowering[S, T]:

  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case Match(matchee, cases) =>
      val alternatives = cases.map { case Case(name, patVars, body) =>
        Deconstruct(matchee, name, patVars) +: body
      }
      Seq(Disjunction((alternatives)))
    case _ => super.visitAtom(atom)
  }
