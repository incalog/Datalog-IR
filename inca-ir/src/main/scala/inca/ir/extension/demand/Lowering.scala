package inca.ir.extension.demand

import inca.ir.Hint.preserveHints
import inca.ir.extension.disjunction.{Disjunction, IR}
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body}

import scala.collection.mutable.ListBuffer

object Lowering:
  def apply[S <: IR, T <: BaseIR](srcIR: S, trgIR: T): Lowering[S, T] = new Lowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

trait Lowering[S <: IR, T <: BaseIR] extends BaseLowering[S, T]:
  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case Demand(ts) =>
        ???
        Seq(atom)
  }
