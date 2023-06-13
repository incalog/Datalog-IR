package inca.ir.lowering

import inca.ir.{Atom, Body, Call, BaseIR, Module}
import inca.ir.visitors.IRVisitor

// TODO: Do we still need S and T ??
trait Lowering[S <: BaseIR, T <: BaseIR] extends IRVisitor {
  def src: S
  def trg: T

  def loweredIRs: Set[BaseIR] = Set()

  def lower(module: Module): Module = {
    if (!module.lang.includes(trg.requires))
      throw new IllegalArgumentException(s"Module $module misses required features: ${trg.requires.features}")
    visit(Module(module.name, module.lang -- loweredIRs, module.contents))
  }
}
